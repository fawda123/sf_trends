######
# data processing for discrete delta stations

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(purrr)

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
# ccf comparison of nitrogen data to flow data for selected delta and suisun stations
# all data are matched by corresponding days in the record
# multiple measures per month were averaged after matching by day
# nitrogen data are nh4, no23, din
# stations are C10, C3, P8, D4, D6, D7

data(delt_dat)
data(flow_dat)

# prep data for ccf by unique site, nut, flow  variables
# data must be in long format
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  select(-east) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  select(-Latitude, -Longitude, -tn) %>% 
  tidyr::gather('resvar', 'resval', din:no23) %>% 
  tidyr::gather('flovar', 'floval', sal:sjr) %>% 
  mutate(flovar = factor(flovar, levels = c('sacyolo', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin'))) %>% 
  group_by(Site_Code, Location, resvar, flovar)

# get ccf by nesting by grouping variable
corests <- nest(toeval) %>% 
  mutate(corest = map(
    data, function(x){
    
      # standardize data as monthly obs for ccf
      dattmp <- mutate(x, 
        resval = log(1 + resval), 
        floval = log(1 + floval), 
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
          nh = log(1 + nh), 
          no23 = log(1 + no23), 
          din = log(1 +din)
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

