rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

# raw data from http://www.water.ca.gov/bdma/meta/Discrete/data.cfm
dat1 <- read_excel('ignore/Lab Data 1975-1984x.xlsx')
dat2 <- read_excel('ignore/Lab Data 1985-1995x.xlsx')
dat3 <- read_excel('ignore/Lab_Data_1996-2012.xlsx')

stats <- c('C10', 'C3', 'P8', 'D19', 'D26', 'D28', 'D4', 'D6', 'D7')
analytes <- c('Ammonia (Total)', 'Ammonia (Dissolved)', 'Nitrite + Nitrate (Dissolved)', 
  'Silica (SiO2) (Dissolved)', 'Chlorophyll a')

# combine data, filter by stations/analytes
dat <- rbind(dat1, dat2, dat3) %>% 
  select(StationCode, SampleDate, ConstituentName, Result, UnitName, ReportingLimit) %>% 
  filter(ConstituentName %in% analytes & StationCode %in% stats) 
  
# unique detection limits by analyte, station, year  
# note that these are only for those that were reported
detlims <- select(dat, SampleDate, ConstituentName, ReportingLimit) %>% 
  mutate(SampleDate = year(SampleDate)) %>% 
  unique %>% 
  na.omit

# detection limits by year
ggplot(detlims, aes(x = SampleDate, y = ReportingLimit, group = ConstituentName, color = ConstituentName)) +
  geom_line() + 
  geom_point(size = 3) + 
  facet_wrap(~ ConstituentName, ncol = 1, scales = 'free_y') + 
  theme_minimal()

# distribution of analytes
ggplot(dat, aes(x = Result, fill = ConstituentName)) + 
  geom_histogram() +
  facet_wrap(~ ConstituentName, ncol = 1, scales = 'free') + 
  theme_minimal()

# chl 0.05
# sio2 0.01
# no23 0.01
# din 0.01
# nh4 0.01 
