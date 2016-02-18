######
# data processing for discrete delta stations

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)

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

# finally, subset dat for active stations in Novick et al. report
dat <- filter(dat, Site_Code %in% c('C3', 'C10', 'P8', 'MD10', 'D19', 'D26', 'D28A', 'D4', 'D8', 'D7', 'D6'))
 
delt_dat <- dat
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
    sacyolo = sac + yolo
  ) %>% 
  select(Date, sacyolo, east, sjr) %>% 
  gather('station', 'q', sacyolo:sjr)
  

save(flow_dat, file = 'data/flow_dat.RData')

######
# matching flow to nutrient time series
# result is a data.frame of each wq station compared with each flow time series
# lags and correlations for each are shown starting with zero lag back to the previous twelve months
# only negative lags are evaluated, i.e, how far back (in months) are the flow time series correlated with nuts
# correlations are based on log-log

rm(list = ls())

load(file = 'data/delt_dat.RData')
load(file = 'data/flow_dat.RData')

# get sites and flow stations to compare
sites <- unique(delt_dat$Site_Code)
flos <- unique(flow_dat$station) 
grd <- expand.grid(sites, flos)
names(grd) <- c('site', 'flos')

# go through each combo and get cross-correlation ests
# requires some grouping by monthly averages
outests <- vector('list', length = nrow(grd))
names(outests) <- paste(grd[, 1], grd[, 2], sep = '_')
for(i in 1:nrow(grd)){
  
  # select grd values to eval
  site <- grd[i, 'site']
  flo <- grd[i, 'flos']
  
  # subset ts and flo data by sites in grd
  sit_sel <- filter(delt_dat, Site_Code == site)
  flo_sel <- filter(flow_dat, station == flo)
  
  # join the flo and wq datasets
  # create complete dataset by year, month, day variables
  # take annual, monthly averages of each parameter
  toeval <- filter(flo_sel, Date >= min(sit_sel$Date) & Date <= max(sit_sel$Date)) %>% 
    full_join(., sit_sel, by = 'Date') %>% 
    select(Date, q, nh, no23) %>% 
    mutate(
      Year = year(Date), 
      Month = month(Date), 
      Day = day(Date),
      q = log(1 + q), 
      nh = log(1 + nh), 
      no23 = log(1 + no23)
    ) %>% 
    select(-Date) %>% 
    complete(Year, Month, Day) %>% 
    group_by(Year, Month) %>% 
    summarize(
      q = mean(q, na.rm = TRUE),
      nh = mean(nh, na.rm = TRUE),
      no23 = mean(no23, na.rm = TRUE)
    ) %>% 
    data.frame
    
  # get ccf
  outnm <- paste(site, flo, sep = '_')
  corest <- ccf(toeval$q, toeval$no23, na.action = na.exclude, lag.max = 12, plot = F)
  corest <- data.frame(nm = outnm, lag = corest$lag, acf = corest$acf, stringsAsFactors = F)
  corest <- corest[corest$lag <= 0, ]
  
  # counter
  percdon <- round(100 * i/nrow(grd), 2)
  cat(outnm, '\t')
  cat(percdon, '%\n')
  outests[[outnm]] <- corest
     
}

# combine the results
flocor <- do.call('rbind', outests) %>% 
  separate(nm, c('site', 'flo'), sep = '_') %>% 
  mutate(lag = factor(lag))
row.names(flocor) <- 1:nrow(flocor)

save(flocor, file = 'data/flocor.RData')

######
# combine flow data with nut data based on matching stations and smoothing 

rm(list = ls())

data(flocor)
data(flow_dat)
data(delt_dat)

# find flow record with min cor for each station
bests <- group_by(flocor, by = site) %>% 
  filter(acf == min(acf)) %>%
  ungroup %>% 
  select(-by) %>% 
  data.frame

# for each site, grab the flow record, lag
# average by left window for the lag
sites <- bests$site
siteflo <- vector('list', length = length(sites))
names(siteflo) <- sites
for(st in sites){
  
  # select the flo station and lag (in months)
  flos <- filter(bests, site == st)
  lag <- abs(as.numeric(as.character(flos$lag))) * 31
  flos <- flos$flo
 
  # filter the flow data by the relevation station, take log of Q, do left mwa foir flow
  floavg <- filter(flow_dat, station == flos) %>% 
    mutate(
      logq = log(1 + q), 
      logqavg = stats::filter(logq, sides = 1, filter = rep(1, lag)/lag, method = 'convolution'),
      logqavg = as.numeric(logqavg)
      ) %>% 
    select(Date, logqavg, logq) %>% 
    mutate(Site_Code = st)
  
  # append to output
  siteflo[[st]] <- na.omit(floavg)
  
}

siteflo <- do.call('rbind', siteflo) %>% 
  data.frame(., stringsAsFactors = F)
row.names(siteflo) <- 1:nrow(siteflo)

# combine the smoothed flow records with nut data for the corresponding station
delt_dat <- left_join(delt_dat, siteflo, by = c('Site_Code', 'Date'))

# save new delt_dat
save(delt_dat, file = 'data/delt_dat.RData')

######
# run winsrch_optim on matched nutrient, flow data
# this ids 'optimal' half-window widths for each station
# this takes several days

library(WRTDStidal)
library(dplyr)

data(delt_dat)

library(doParallel)
ncores <- detectCores() - 2  
registerDoParallel(cores = ncores)

sites <- unique(delt_dat$Site_Code)
mods_opt <- vector('list', length = length(sites))
names(mods_opt) <- sites
for(site in sites){
  
  cat(site, '\n')
  
  # data prep
  tomod <- filter(delt_dat, Site_Code == site) %>% 
    select(Date, no23, logqavg) %>% 
    mutate(
      lim = -1e6,
      no23 = log(1 + no23)
      ) %>% 
    data.frame %>% 
    tidal(., 
      reslab = expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')')), 
      flolab = expression(paste('ln-flow (standardized)'))
    )
  
  mod <- winsrch_optim(tomod, tau = 0.5)
  
  mods_opt[[site]] <- mod

  save(mods_opt, file = 'data/mods_opt.RData')
  
}

    

