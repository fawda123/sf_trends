# ######
# # data processing for discrete delta stations

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(purrr)
library(WRTDStidal)
library(foreach)
library(doParallel)

######
# wq data

load(file = 'ignore/dwr_wq.RData')

statmeta <- read_excel('ignore/Discrete_Stations.xls')
curr <- filter(statmeta, Historic_or_Current == 'Current') %>% 
  .$Site_Code %>% 
  unique
hist <- filter(statmeta, Historic_or_Current == 'Historic') %>% 
  .$Site_Code %>% 
  unique

## Combine C10 with C10A, C3 with C3A, P12 with P12A, P10 with P10A, MD10 with MD10A, MD7 with MD7A
# the following three have to be added manually to curr because they were combined in original processing script from Novick
curr <- c(curr, 'C10', 'C3', 'MD10')  

# add lat/long from meta
# only get current stations
# remove EZ2/EZ6 (no lat/long data, TN record only back to 2010)
# remove NZ stations, no nitrogen data
# select ammonia, nitrate, dissolved inorganic nitrogen, total nitrogen (all as mg/L), and salinity (psu probably)
dat <- select(statmeta, Site_Code, Latitude, Longitude) %>% 
  na.omit %>% 
  right_join(., dwr_wq, by = c('Site_Code' = 'Station')) %>% 
  filter(Site_Code %in% curr) %>% 
  filter(!Site_Code %in% c('EZ2', 'EZ6', 'NZ002', 'NZ004', 'NZ032', 'NZ325', 'NZS42')) %>% 
  select(Site_Code, Date, Latitude, Longitude, nh, no23, din, tn, sal)

# finally, subset by selected delta and suisun stations
# make sure each date is unique
dat <- filter(dat, Site_Code %in% c('C3', 'C10', 'P8', 'D4', 'D7', 'D6')) %>% 
  mutate(
    year = year(Date), 
    month = month(Date), 
    day = day(Date)
  ) %>% 
  filter(year > 1975 & year < 2014) %>% 
  gather('var', 'val', Latitude:sal) %>% 
  group_by(Site_Code, year, month, day, var) %>% 
  summarize(val = mean(val, na.rm = TRUE)) %>% 
  spread(var, val) %>% 
  ungroup %>% 
  mutate(
    Date = as.Date(paste(year, month, day, sep = '-'), format = '%Y-%m-%d'),
    Location = 'Delta'
    ) %>% 
  select(Site_Code, Location, Date, Latitude, Longitude, din, nh, no23, tn, sal)
dat$Location[dat$Site_Code %in%  c('D4', 'D7', 'D6')] <- 'Suisun'

delt_dat <- dat
save(delt_dat, file = 'data/delt_dat.RData')

######
# remove outliers (from viz in ggpairs, original readme)

data(delt_dat)

tokp <- with(delt_dat, !(Site_Code == 'C10' & sal > (exp(1.5) - 1)) | is.na(sal))
delt_dat <- filter(delt_dat, tokp)
tokp <- with(delt_dat, !(Site_Code == 'D4' & no23 > (exp(0.9) - 1)) & !(Site_Code == 'D4' & din > (exp(0.9) - 1)) | is.na(no23) | is.na(din))
delt_dat <- filter(delt_dat, tokp)
tokp <- with(delt_dat, !(Site_Code == 'D6' & no23 > (exp(0.75) - 1)) & !(Site_Code == 'D6' & nh > (exp(0.35) - 1)) | is.na(no23) | is.na(nh))
delt_dat <- filter(delt_dat, tokp)
tokp <- with(delt_dat, !(Site_Code == 'D7' & no23 > (exp(0.9) - 1)) | is.na(no23))
delt_dat <- filter(delt_dat, tokp)

save(delt_dat, file = 'data/delt_dat.RData')

######
# flow records

# inputs and outputs for different locations, cubic feet per second
# input stations retained, converted to m3/s
flow_dat <- read.csv('ignore/DAYFLOW_1975_2015.csv') %>% 
  select(DATE, SAC, YOLO, CSMR, MOKE, MISC, SJR, EAST, TOT, XGEO, WEST, PREC, SJR) %>% 
  mutate(
    DATE = as.character(DATE), 
    DATE = as.Date(DATE, format = '%m/%d/%y')
  ) %>% 
  gather('var', 'val', -DATE) %>% 
  mutate(
    val = val * 0.028316847,
    var = tolower(var)
    ) %>% 
  rename(Date = DATE)

# pull out input stations from Novick et al, combine based on fig 2
flow_dat <- filter(flow_dat, var %in% c('sjr', 'sac', 'yolo', 'csmr', 'moke', 'misc')) %>% 
  spread(var, val) %>% 
  mutate(
    east = csmr + moke + misc,
    sac = sac
  ) %>% 
  select(Date, sac, east, sjr) %>% 
  gather('station', 'q', sac:sjr)

save(flow_dat, file = 'data/flow_dat.RData')

######
# ccf comparison of nitrogen data to flow data for selected delta and suisun stations
# all data are matched by corresponding days in the record
# multiple measures per month were averaged after matching by day
# nitrogen data are nh4, no23, din
# stations are C10, C3, P8, D4, D6, D7

data(delt_dat)
data(flow_dat)

# prep data for ccf by unique site, nut, flow  variables
# data must be in long format
# salinity is the only variable with zero, add + 1
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  select(-east) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  select(-Latitude, -Longitude, -tn) %>% 
  mutate(sal = 1 + sal) %>% 
  tidyr::gather('resvar', 'resval', din:no23) %>% 
  tidyr::gather('flovar', 'floval', sal:sjr) %>% 
  mutate(flovar = factor(flovar, levels = c('sac', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin'))) %>% 
  group_by(Site_Code, Location, resvar, flovar)

# get ccf by nesting by grouping variable
corests <- nest(toeval) %>% 
  mutate(corest = map(
    data, function(x){
    
      # standardize data as monthly obs for ccf
      dattmp <- mutate(x, 
        resval = log(resval), 
        floval = log(floval), 
        Year = year(Date), 
        Month = month(Date)
        ) %>% 
        select(-Date) %>% 
        complete(Year, Month) %>% 
        group_by(Year, Month) %>% 
        summarize(
          resval = mean(resval, na.rm = TRUE),
          floval = mean(floval, na.rm = TRUE)
        ) %>% 
        data.frame
        
      # get ccf
      corest <- with(dattmp, ccf(floval, resval,na.action = na.exclude, lag.max = 12, plot = F))
      corest <- data.frame(lag = corest$lag, acf = corest$acf, stringsAsFactors = F)
      
      # exit
      return(corest)

    })
  )

# unnest by corest
corests <- unnest(corests, corest) %>% 
  data.frame

flocor <- corests
save(flocor, file = 'data/flocor.RData', compress = 'xz')

######
# nutrients v nutrients, similar analysis as above but ccf of nutrient data to each other

rm(list = ls())

data(delt_dat)
data(flow_dat)

toeval <- select(delt_dat, -Latitude, -Longitude, -sal, -tn) %>% 
  group_by(Site_Code, Location)

corests <- nest(toeval) %>% 
  mutate(corest = map(data, 
    function(x){
      
      dattmp <- mutate(x, 
          Year = year(Date), 
          Month = month(Date), 
          Day = day(Date),
          nh = log(nh), 
          no23 = log(no23), 
          din = log(din)
        ) %>% 
        select(-Date) %>% 
        complete(Year, Month, Day) %>% 
        group_by(Year, Month) %>% 
        summarize(
          nh = mean(nh, na.rm = TRUE),
          no23 = mean(no23, na.rm = TRUE), 
          din = mean(din, na.rm = TRUE)
        ) %>% 
        data.frame
    
      # this is terrible
      dinnh <- with(dattmp, ccf(din, nh, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      dinno23 <- with(dattmp, ccf(din, no23, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf)) 
      nhdin <- with(dattmp, ccf(nh, din, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      nhno23 <- with(dattmp, ccf(nh, no23, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      no23din <- with(dattmp, ccf(no23, din, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      no23nh <- with(dattmp, ccf(no23, nh, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf)) 
      nhdin <- with(dattmp, ccf(nh, din, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      no23din <- with(dattmp, ccf(no23, din, na.action = na.exclude, lag.max = 12, plot = F)) %>% 
        with(., data.frame(lag = lag, acf = acf))
      
      out <- list(
        din_nh = dinnh, 
        din_no23 = dinno23, 
        nh_din = nhdin, 
        nh_no23 = nhno23, 
        no23_din = no23din, 
        no23_nh = no23nh, 
        nh_din = nhdin, 
        no23_din = no23din
        )
      
      out <- reshape2::melt(out, id.vars = c('lag', 'acf'))
        
      return(out)
      
    })
  )

corests <- unnest(corests, corest) %>% 
  separate(L1, c('var1', 'var2'), sep = '_') %>% 
  data.frame

nutcor <- corests
save(nutcor, file = 'data/nutcor.RData')

######
# min or max cor of each nut parm with flow or sal time series, respectively
# each site is matched with the flow or salinity record ided in figure 10 of Novick et al

data(flocor)

# lookup table for flow, nutrient recs
floref <- data.frame(
  Site_Code = c('C10', 'C3', 'P8', 'D4', 'D6', 'D7'), 
  flovar = c('San Joaquin', 'Sacramento', 'San Joaquin', 'Salinity', 'Salinity', 'Salinity'), 
  stringsAsFactors = F
  )

# iterate through floref 
bests <- NULL
for(i in 1:nrow(floref)){
  
  site <- floref[i, 'Site_Code']
  flo <- floref[i, 'flovar']
  
  toeval <- filter(flocor, Site_Code == site & flovar == flo) %>% 
    filter(lag <= 0) %>% 
    group_by(resvar)
  
  # get max cor if salinity
  if(flo == 'Salinity'){
  
    outest <- filter(toeval, max(acf) == acf)
    
  # otherwise min cor  
  } else {
  
    outest <- filter(toeval, min(acf) == acf)
      
  }
  
  bests <- rbind(bests, outest)
  
}

bests <- as.data.frame(bests, stringsAsFactors = FALSE)

save(bests, file = 'data/bests.RData', compress = 'xz')
 
######
# dataset for wrtds, all response, flow values are ln transformed, flow (or salinity) records for each nutrient variable and station are combined based on the monthly lag ided from bests.RData

data(delt_dat)
data(flow_dat)
data(bests)

# prep data for combining, have to get data as monthly averages for lag combos
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  select(-east) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  select(-Latitude, -Longitude, -tn) %>%  
  tidyr::gather('resvar', 'resval', din:no23) %>% 
  tidyr::gather('flovar', 'floval', sal:sjr) %>% 
  mutate(
    flovar = factor(flovar, levels = c('sac', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin')), 
    year = year(Date), 
    month = month(Date)
    ) %>% 
  select(-Date) %>% 
  group_by(Site_Code, Location, resvar, flovar) %>% 
  complete(year, month) %>% 
  group_by(Site_Code, Location, resvar, flovar, year, month) %>%   
  summarize(
    resval = mean(resval, na.rm = TRUE),
    floval = mean(floval, na.rm = TRUE)
    ) %>% 
  mutate(
    day = 1,
    Date = paste(year, month, day, sep = '-'), 
    Date = as.Date(Date)
    ) %>% 
  ungroup %>% 
  select(Location, Site_Code, Date, resvar, resval, flovar, floval)

# iterate through the lag combos, for each site and nut
# bump up the flow record by the month lags, if any
out <- NULL
for(i in 1:nrow(bests)){
  
  site <- bests[i, 'Site_Code'] 
  flo <- bests[i, 'flovar']
  res <- bests[i, 'resvar']
  lag <- abs(bests[i, 'lag'])
  
  tocomb <- filter(toeval, Site_Code == site & flovar == flo & resvar == res)

  florec <- tocomb$floval
  florec <- c(rep(NA, lag), florec)
  n <- length(florec)
  florec <- florec[c(1:(n - lag))]
  tocomb$flolag <- florec
  
  out <- rbind(out, tocomb)
  
}

# add + 1 to salinity, this is the only variable with zeroes
out[out$flovar == 'Salinity', 'flolag'] <- 1 + out[out$flovar == 'Salinity', 'flolag']

# log transform
out <- select(out, -floval) %>% 
  mutate(
    resval = log(resval),
    flolag = log(flolag),
    lim = -1e6
  )

mods_lag <- out

save(mods_lag, file = 'data/mods_lag.RData', compress = 'xz')

######
# fit models with default window widths
# get predictions with daily flow records

cl <- makeCluster(7)
registerDoParallel(cl)

data(mods_lag)

mods_lag <- mutate(mods_lag, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest 

# resvar label lookup
lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
    )
  )

# flovar label lookup
stalk <- list(
  shrt = c('sjr', 'sac', 'sal'),
  lngs = c('San Joaquin', 'Sacramento', 'Salinity')
  )

strt <- Sys.time()

# iterate through stations, res vars to model
# get predictions from obs time series of salinity or flow
mods_out <- foreach(i = 1:nrow(mods_lag)) %dopar% {
  
  data(flow_dat)
  data(delt_dat)
  
  library(dplyr)
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(mods_lag), '\n')
  print(Sys.time()-strt)
  sink()
  
  # data, response variable label
  dat <- mods_lag[i, ]$data[[1]]
  resvar <- mods_lag[i, ]$resvar
  flovar <- mods_lag[i, ]$flovar
  sta <- mods_lag[i, ]$Site_Code
  reslab <- with(lablk, lngs[shrt == resvar])
  flolab <- with(stalk, shrt[lngs == flovar])
  
  # prep data as tidal object
  tomod <- select(dat, Date, resval, flolag, lim) %>% 
    rename(
      res = resval, 
      flo = flolag
    ) %>% 
    data.frame %>% 
    tidal(., 
      reslab = reslab, 
      flolab = expression(paste('ln-flow (standardized)'))
    )

  # get flo or salinity variable to predict 
  if(flolab == 'sal'){
    
    topred <- filter(delt_dat, Site_Code == sta) %>% 
      mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
      rename(date = Date) %>% 
      select(date, flo) %>% 
      filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
      na.omit %>% 
      data.frame
    
  } else {
    
    topred <- filter(flow_dat, station == flolab) %>% 
      mutate(flo = log(q)) %>% 
      rename(date = Date) %>% 
      select(date, flo) %>% 
      filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
      na.omit %>% 
      data.frame
    
  }
  
  # create model and exit
  mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150)
  
  # get predictions, norms from obs flow data
  out <- mod %>% 
    respred(dat_pred = topred) %>% 
    resnorm

  # assign to unique object, save in case of fuckery
  outnm <- paste0(sta, '_', resvar)
  assign(outnm, out)
  save(list = outnm, file = paste0('data/', outnm, '.RData'), compress = 'xz')
  
  # out for list
  out
  
}

# import each file, add to nested mods_lag dataframe
fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7', full.names = T)
dat <- lapply(fls, load, .GlobalEnv)
names(dat) <- unlist(dat)
dat <- lapply(dat, get)

mods_lag <- unite(mods_lag, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(mod = dat[match(tmp, names(dat))]) %>% 
  select(-tmp)

# remove the individual files
file.remove(fls)

# save output
save(mods_lag, file = 'data/mods_lag.RData', compress = 'xz')

######
# dataset for wrtds, all response, flow values are ln transformed
# same as above except flow is not matched with the max lag, dates matched instead
# the code is identical, just a stupid hack to set max lag at zero

rm(list = ls())

data(delt_dat)
data(flow_dat)
data(bests)

# prep data for combining, have to get data as monthly averages for lag combos
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  select(-east) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  select(-Latitude, -Longitude, -tn) %>% 
  tidyr::gather('resvar', 'resval', din:no23) %>% 
  tidyr::gather('flovar', 'floval', sal:sjr) %>% 
  mutate(
    flovar = factor(flovar, levels = c('sac', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin')), 
    year = year(Date), 
    month = month(Date)
    ) %>% 
  select(-Date) %>% 
  group_by(Site_Code, Location, resvar, flovar) %>% 
  complete(year, month) %>% 
  group_by(Site_Code, Location, resvar, flovar, year, month) %>%   
  summarize(
    resval = mean(resval, na.rm = TRUE),
    floval = mean(floval, na.rm = TRUE)
    ) %>% 
  mutate(
    day = 1,
    Date = paste(year, month, day, sep = '-'), 
    Date = as.Date(Date)
    ) %>% 
  ungroup %>% 
  select(Location, Site_Code, Date, resvar, resval, flovar, floval)

# iterate through the lag combos, for each site and nut
# bump up the flow record by the month lags, if any
out <- NULL
for(i in 1:nrow(bests)){
  
  site <- bests[i, 'Site_Code'] 
  flo <- bests[i, 'flovar']
  res <- bests[i, 'resvar']
  lag <- 0 # this part changed
  
  tocomb <- filter(toeval, Site_Code == site & flovar == flo & resvar == res)

  florec <- tocomb$floval
  florec <- c(rep(NA, lag), florec)
  n <- length(florec)
  florec <- florec[c(1:(n - lag))]
  tocomb$flolag <- florec
  
  out <- rbind(out, tocomb)
  
}

# add + 1 to salinity, this is the only variable with zeroes
out[out$flovar == 'Salinity', 'flolag'] <- 1 + out[out$flovar == 'Salinity', 'flolag']

out <- select(out, -floval) %>% 
  mutate(
    resval = log(resval),
    flolag = log(flolag),
    lim = -1e6
  )

mods_nolag <- out

save(mods_nolag, file = 'data/mods_nolag.RData', compress = 'xz')

######
# fit models with default window widths
# get predictions with daily flow records

cl <- makeCluster(7)
registerDoParallel(cl)

data(mods_nolag)

mods_nolag <- mutate(mods_nolag, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest 

# resvar label lookup
lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
    )
  )

# flovar label lookup
stalk <- list(
  shrt = c('sjr', 'sac', 'sal'),
  lngs = c('San Joaquin', 'Sacramento', 'Salinity')
  )

strt <- Sys.time()

# iterate through stations, res vars to model
# get predictions from obs time series of salinity or flow
mods_out <- foreach(i = 1:nrow(mods_nolag)) %dopar% {
  
  data(flow_dat)
  data(delt_dat)
  
  library(dplyr)
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(mods_nolag), '\n')
  print(Sys.time()-strt)
  sink()
  
  # data, respons variable label
  dat <- mods_nolag[i, ]$data[[1]]
  resvar <- mods_nolag[i, ]$resvar
  flovar <- mods_nolag[i, ]$flovar
  sta <- mods_nolag[i, ]$Site_Code
  reslab <- with(lablk, lngs[shrt == resvar])
  flolab <- with(stalk, shrt[lngs == flovar])
  
  # prep data as tidal object
  tomod <- select(dat, Date, resval, flolag, lim) %>% 
    rename(
      res = resval, 
      flo = flolag
    ) %>% 
    data.frame %>% 
    tidal(., 
      reslab = reslab, 
      flolab = expression(paste('ln-flow (standardized)'))
    )

  # get flo or salinity variable to predict 
  if(flolab == 'sal'){
    
    topred <- filter(delt_dat, Site_Code == sta) %>% 
      mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
      rename(date = Date) %>% 
      select(date, flo) %>% 
      filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
      na.omit %>% 
      data.frame
    
  } else {
    
    topred <- filter(flow_dat, station == flolab) %>% 
      mutate(flo = log(q)) %>% 
      rename(date = Date) %>% 
      select(date, flo) %>% 
      filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
      na.omit %>% 
      data.frame
    
  }
  
  # create model and exit
  mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150)
  
  # get predictions, norms from obs flow data
  out <- mod %>% 
    respred(dat_pred = topred) %>% 
    resnorm

  # assign to unique object, save in case of fuckery
  outnm <- paste0(sta, '_', resvar)
  assign(outnm, out)
  save(list = outnm, file = paste0('data/', outnm, '.RData'), compress = 'xz')
  
  # out for list
  out
  
}

# import each file, add to nested mods_nolag dataframe
fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7', full.names = T)
dat <- lapply(fls, load, .GlobalEnv)
names(dat) <- unlist(dat)
dat <- lapply(dat, get)

mods_nolag <- unite(mods_nolag, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(mod = dat[match(tmp, names(dat))]) %>% 
  select(-tmp)

# remove the individual files
file.remove(fls)

# save output
save(mods_nolag, file = 'data/mods_nolag.RData', compress = 'xz')
