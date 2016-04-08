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
# min or max cor of each nut parm with flow or sal time series, respectively
# each site is matched with the flow or salinity record ided in figure 10 of Novick et al

data(flocor)

# lookup table for flow, nutrient recs
floref <- data.frame(
  Site_Code = c('C10', 'C3', 'P8', 'D4', 'D6', 'D7', 'D19', 'D26', 'D28'), 
  flovar = c('San Joaquin', 'Sacramento', 'San Joaquin', 'Salinity', 'Salinity', 'Salinity', 'Sacramento', 'Sacramento', 'Sacramento'), 
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

mods_lag_tmp <- out

save(mods_lag_tmp, file = 'data/mods_lag_tmp.RData', compress = 'xz')

######
# fit models with default window widths
# get predictions with daily flow records

cl <- makeCluster(4)
registerDoParallel(cl)

data(mods_lag_tmp)

mods_lag_tmp <- mutate(mods_lag_tmp, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest %>% 
  filter(Site_Code %in% 'D19')

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
mods_out <- foreach(i = 1:nrow(mods_lag_tmp)) %dopar% {
  
  data(flow_dat)
  data(delt_dat)
  
  library(dplyr)
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(mods_lag_tmp), '\n')
  print(Sys.time()-strt)
  sink()
  
  # data, response variable label
  dat <- mods_lag_tmp[i, ]$data[[1]]
  resvar <- mods_lag_tmp[i, ]$resvar
  flovar <- mods_lag_tmp[i, ]$flovar
  sta <- mods_lag_tmp[i, ]$Site_Code
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

    # if D19, remove gap in time series with no data
    if(sta == 'D19'){
      
      dts <- as.Date(c('1995-12-01', '2004-05-01')) # looked at orig record for this
      
      torm <- with(topred, date > dts[1] & date < dts[2])
      topred$flo[torm] <- NA
      
    }
        
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

# import each file, add to nested mods_lag_tmp dataframe
fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7|^D19|^D26|^D28', full.names = T)
dat <- lapply(fls, load, .GlobalEnv)
names(dat) <- unlist(dat)
dat <- lapply(dat, get)

mods_lag_tmp <- unite(mods_lag_tmp, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(mod = dat[match(tmp, names(dat))]) %>% 
  select(-tmp)

# remove the individual files
file.remove(fls)

# save output
save(mods_lag_tmp, file = 'data/mods_lag_tmp.RData', compress = 'xz')

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

mods_nolag_tmp <- out

save(mods_nolag_tmp, file = 'data/mods_nolag_tmp.RData', compress = 'xz')

######
# fit models with default window widths
# get predictions with daily flow records

cl <- makeCluster(4)
registerDoParallel(cl)

data(mods_nolag_tmp)

mods_nolag_tmp <- mutate(mods_nolag_tmp, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest%>% 
  filter(Site_Code %in% 'D19') 

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
mods_out <- foreach(i = 1:nrow(mods_nolag_tmp)) %dopar% {
  
  data(flow_dat)
  data(delt_dat)
  
  library(dplyr)
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(mods_nolag_tmp), '\n')
  print(Sys.time()-strt)
  sink()
  
  # data, respons variable label
  dat <- mods_nolag_tmp[i, ]$data[[1]]
  resvar <- mods_nolag_tmp[i, ]$resvar
  flovar <- mods_nolag_tmp[i, ]$flovar
  sta <- mods_nolag_tmp[i, ]$Site_Code
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
    
    # if D19, remove gap in time series with no data
    if(sta == 'D19'){
      
      dts <- as.Date(c('1995-12-01', '2004-05-01')) # looked at orig record for this
      
      torm <- with(topred, date > dts[1] & date < dts[2])
      topred$flo[torm] <- NA
      
    }
    
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

# import each file, add to nested mods_nolag_tmp dataframe
fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7|^D19|^D26|^D28', full.names = T)
dat <- lapply(fls, load, .GlobalEnv)
names(dat) <- unlist(dat)
dat <- lapply(dat, get)

mods_nolag_tmp <- unite(mods_nolag_tmp, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(mod = dat[match(tmp, names(dat))]) %>% 
  select(-tmp)

# remove the individual files
file.remove(fls)

# save output
save(mods_nolag_tmp, file = 'data/mods_nolag_tmp.RData', compress = 'xz')

######
# add to existing data

# load all
data(mods_lag)
data(mods_nolag)
data(mods_lag_tmp)
data(mods_nolag_tmp)

# replace old D19 with new D19
mods_lag[mods_lag$Site_Code %in% 'D19', ] <- mods_lag_tmp
mods_nolag[mods_nolag$Site_Code %in% 'D19', ] <- mods_nolag_tmp

# save
save(mods_lag, file = 'data/mods_lag.RData')
save(mods_nolag, file = 'data/mods_nolag.RData')