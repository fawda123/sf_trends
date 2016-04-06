######
# data processing for discrete delta stations

library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(purrr)
library(WRTDStidal)
library(foreach)
library(doParallel)
library(dplyr)

rm(list = ls())

data(delt_dat)
data(flow_dat)
data(bests)

# prep data for combining, have to get data as monthly averages for lag combos
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  select(-Latitude, -Longitude, -tn, -sal) %>% 
  filter(Site_Code %in% c('D28', 'D26', 'D19')) %>% 
  tidyr::gather('resvar', 'resval', din:no23) %>% 
  tidyr::gather('flovar', 'floval', east:sjr) %>% 
  mutate(
    flovar = factor(flovar, levels = c('sac', 'east', 'sjr'), labels = c('Sacramento', 'East', 'San Joaquin')), 
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

####
# combos to eval
sites <- unique(toeval$Site_Code)
flows <- unique(toeval$flovar)
resps <- unique(toeval$resvar)
grds <- expand.grid(sites, flows, resps)
names(grds) <- c('sites', 'flows', 'resps')

# resvar label lookup
lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
    )
  )

# setup parallel backend
cl <- makeCluster(7)
registerDoParallel(cl)
strt <- Sys.time()

# iterate through combos to fit mod
out <- foreach(i = 1:nrow(grds)) %dopar% {
  
  library(dplyr)
  library(WRTDStidal)
  
  # counter
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(grds), '\n')
  print(Sys.time()-strt)
  sink()
  
  # get info for site, flow, response
  site <- grds[i, 'sites'] 
  flo <- grds[i, 'flows']
  res <- grds[i, 'resps']
  reslab <- with(lablk, lngs[shrt == res])
  
  # subset data
  tomod <- filter(toeval, Site_Code == site & flovar == flo & resvar == res) %>% 
    select(Date, resval, floval) %>% 
    mutate(
      resval = log(resval), 
      floval = log(floval), 
      lim = -1e6
    ) %>% 
    data.frame %>% 
    tidal(., 
      reslab = reslab, 
      flolab = expression(paste('ln-flow (standardized)'))
    )

  # create model
  mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 10, 0.5), flo_div = 30, min_obs = 150) %>% 
    respred %>% 
    resnorm
  
  return(mod)
  
}

# format output
grds <- unite(grds, 'nms', sites:resps, sep = '_')
names(out) <- grds$nms
out <- lapply(out, wrtdsperf) %>% 
  reshape2::melt(id.vars = names(.[[1]])) %>% 
  separate(L1, c('Site_Code', 'flovar', 'resvar'), sep = '_')

# save 
middle_fits <- out
save(middle_fits, file = 'data/middle_fits.RData', compress = 'xz')
