######
# data processing for discrete delta stations

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

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



