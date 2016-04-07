
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

cl <- makeCluster(7)
registerDoParallel(cl)

data(mods_lag_tmp)

mods_lag_tmp <- mutate(mods_lag_tmp, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest %>% 
  .[19:27, ]

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

# ######
# # dataset for wrtds, all response, flow values are ln transformed
# # same as above except flow is not matched with the max lag, dates matched instead
# # the code is identical, just a stupid hack to set max lag at zero
# 
# rm(list = ls())
# 
# data(delt_dat)
# data(flow_dat)
# data(bests)
# 
# # prep data for combining, have to get data as monthly averages for lag combos
# toeval <- tidyr::spread(flow_dat, station, q) %>% 
#   select(-east) %>% 
#   left_join(delt_dat, ., by = 'Date') %>% 
#   select(-Latitude, -Longitude, -tn) %>% 
#   tidyr::gather('resvar', 'resval', din:no23) %>% 
#   tidyr::gather('flovar', 'floval', sal:sjr) %>% 
#   mutate(
#     flovar = factor(flovar, levels = c('sac', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin')), 
#     year = year(Date), 
#     month = month(Date)
#     ) %>% 
#   select(-Date) %>% 
#   group_by(Site_Code, Location, resvar, flovar) %>% 
#   complete(year, month) %>% 
#   group_by(Site_Code, Location, resvar, flovar, year, month) %>%   
#   summarize(
#     resval = mean(resval, na.rm = TRUE),
#     floval = mean(floval, na.rm = TRUE)
#     ) %>% 
#   mutate(
#     day = 1,
#     Date = paste(year, month, day, sep = '-'), 
#     Date = as.Date(Date)
#     ) %>% 
#   ungroup %>% 
#   select(Location, Site_Code, Date, resvar, resval, flovar, floval)
# 
# # iterate through the lag combos, for each site and nut
# # bump up the flow record by the month lags, if any
# out <- NULL
# for(i in 1:nrow(bests)){
#   
#   site <- bests[i, 'Site_Code'] 
#   flo <- bests[i, 'flovar']
#   res <- bests[i, 'resvar']
#   lag <- 0 # this part changed
#   
#   tocomb <- filter(toeval, Site_Code == site & flovar == flo & resvar == res)
# 
#   florec <- tocomb$floval
#   florec <- c(rep(NA, lag), florec)
#   n <- length(florec)
#   florec <- florec[c(1:(n - lag))]
#   tocomb$flolag <- florec
#   
#   out <- rbind(out, tocomb)
#   
# }
# 
# # add + 1 to salinity, this is the only variable with zeroes
# out[out$flovar == 'Salinity', 'flolag'] <- 1 + out[out$flovar == 'Salinity', 'flolag']
# 
# out <- select(out, -floval) %>% 
#   mutate(
#     resval = log(resval),
#     flolag = log(flolag),
#     lim = -1e6
#   )
# 
# mods_nolag_tmp <- out
# 
# save(mods_nolag_tmp, file = 'data/mods_nolag_tmp.RData', compress = 'xz')
# 
# ######
# # fit models with default window widths
# # get predictions with daily flow records
# 
# cl <- makeCluster(7)
# registerDoParallel(cl)
# 
# data(mods_nolag_tmp)
# 
# mods_nolag_tmp <- mutate(mods_nolag_tmp, resdup = resvar) %>% 
#   group_by(Location, Site_Code, resvar, flovar) %>% 
#   nest %>% 
#   .[19:27, ]
# 
# # resvar label lookup
# lablk <- list(
#   shrt = c('din', 'nh', 'no23'),
#   lngs = c(
#     expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
#     expression(paste('ln-ammonium (mg ', L^-1, ')')),
#     expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
#     )
#   )
# 
# # flovar label lookup
# stalk <- list(
#   shrt = c('sjr', 'sac', 'sal'),
#   lngs = c('San Joaquin', 'Sacramento', 'Salinity')
#   )
# 
# strt <- Sys.time()
# 
# # iterate through stations, res vars to model
# # get predictions from obs time series of salinity or flow
# mods_out <- foreach(i = 1:nrow(mods_nolag_tmp)) %dopar% {
#   
#   data(flow_dat)
#   data(delt_dat)
#   
#   library(dplyr)
#   library(WRTDStidal)
#   
#   sink('C:/Users/mbeck/Desktop/log.txt')
#   cat(i, 'of', nrow(mods_nolag_tmp), '\n')
#   print(Sys.time()-strt)
#   sink()
#   
#   # data, respons variable label
#   dat <- mods_nolag_tmp[i, ]$data[[1]]
#   resvar <- mods_nolag_tmp[i, ]$resvar
#   flovar <- mods_nolag_tmp[i, ]$flovar
#   sta <- mods_nolag_tmp[i, ]$Site_Code
#   reslab <- with(lablk, lngs[shrt == resvar])
#   flolab <- with(stalk, shrt[lngs == flovar])
#   
#   # prep data as tidal object
#   tomod <- select(dat, Date, resval, flolag, lim) %>% 
#     rename(
#       res = resval, 
#       flo = flolag
#     ) %>% 
#     data.frame %>% 
#     tidal(., 
#       reslab = reslab, 
#       flolab = expression(paste('ln-flow (standardized)'))
#     )
# 
#   # get flo or salinity variable to predict 
#   if(flolab == 'sal'){
#     
#     topred <- filter(delt_dat, Site_Code == sta) %>% 
#       mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
#       rename(date = Date) %>% 
#       select(date, flo) %>% 
#       filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
#       na.omit %>% 
#       data.frame
#     
#   } else {
#     
#     topred <- filter(flow_dat, station == flolab) %>% 
#       mutate(flo = log(q)) %>% 
#       rename(date = Date) %>% 
#       select(date, flo) %>% 
#       filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
#       na.omit %>% 
#       data.frame
#     
#   }
#   
#   # create model and exit
#   mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150)
#   
#   # get predictions, norms from obs flow data
#   out <- mod %>% 
#     respred(dat_pred = topred) %>% 
#     resnorm
# 
#   # assign to unique object, save in case of fuckery
#   outnm <- paste0(sta, '_', resvar)
#   assign(outnm, out)
#   save(list = outnm, file = paste0('data/', outnm, '.RData'), compress = 'xz')
#   
#   # out for list
#   out
#   
# }
# 
# # import each file, add to nested mods_nolag_tmp dataframe
# fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7|^D19|^D26|^D28', full.names = T)
# dat <- lapply(fls, load, .GlobalEnv)
# names(dat) <- unlist(dat)
# dat <- lapply(dat, get)
# 
# mods_nolag_tmp <- unite(mods_nolag_tmp, 'tmp', Site_Code, resvar, remove = F) %>% 
#   mutate(mod = dat[match(tmp, names(dat))]) %>% 
#   select(-tmp)
# 
# # remove the individual files
# file.remove(fls)
# 
# # save output
# save(mods_nolag_tmp, file = 'data/mods_nolag_tmp.RData', compress = 'xz')
# 
# ######
# 
# data(mods_nolag)
# data(mods_lag)
# data(mods_nolag_tmp)
# data(mods_lag_tmp)
# 
# mods_nolag <- mods_nolag[1:18, ]
# mods_nolag <- rbind(mods_nolag, mods_nolag_tmp)
# 
# 
# mods_lag <- mods_lag[1:18, ]
# mods_lag <- rbind(mods_lag, mods_lag_tmp)
# 
# save(mods_nolag, file = 'data/mods_nolag.RData', compress = 'xz')
