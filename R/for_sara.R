# load packages
library(mgcv)
library(dplyr)
library(tidyr)
library(lubridate)
library(WRTDStidal)

# for plot function at bottom
source('https://raw.githubusercontent.com/fawda123/sf_trends/master/R/funcs.R')
  
# load all model data
# this is a nested data frame, https://blog.rstudio.org/2016/02/02/tidyr-0-4-0/
load(file = url('https://github.com/fawda123/sf_trends/blob/master/data/mods_nolag.RData?raw=true'))

# subset P8 station from all data
# format columns similar to WRTDS
tmp <- mods_nolag$data[[8]] %>% 
  mutate(
    dec_time = dec_time(Date)[['dec_time']],
    doy = yday(Date)
    ) %>% 
  rename(
    res = resval, 
    flo = flolag,
    date = Date
  )

# create gam 
gamtmp <- gam(res ~ te(dec_time, doy, flo, bs = c("tp", "cc", "tp")), k = c(5, 8, 5), data = tmp, knots = list(doy = c(1, 366)))

# make a plot
# same function as dynaplot in WRTDStidal package but for GAMs
dynagam(gamtmp, tmp)

