library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

#setup parallel backend to use 8 processors
data(delt_dat)
data(flow_dat)

# prep data for ccf by unique site, nut, flow  variables
# data must be in long format
toeval <- tidyr::spread(flow_dat, station, q) %>% 
  select(-east) %>% 
  left_join(delt_dat, ., by = 'Date') %>% 
  filter(Site_Code %in% c('D6', 'D7', 'D4', 'C3', 'P8', 'C10')) %>% 
  select(-Latitude, -Longitude, -tn) %>% 
  tidyr::gather('resvar', 'resval', nh:din) %>% 
  tidyr::gather('flovar', 'floval', sal:sjr) %>% 
  mutate(flovar = factor(flovar, levels = c('sacyolo', 'sal', 'sjr'), labels = c('Sacramento', 'Salinity', 'San Joaquin'))) %>% 
  group_by(Site_Code, resvar, flovar)

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
  mutate(location = 'Delta') %>% 
  data.frame
corests$location[corests$Site_Code %in% c('D7', 'D6', 'D4')] <- 'Suisun'

ggplot(corests, aes(x = lag, y = acf, colour = flovar, group = flovar)) +
  facet_grid(location + Site_Code ~ resvar) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous('lag (months)') +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())

##
# nutrients v nutrients

toeval <- filter(delt_dat, Site_Code %in% c('D6', 'D7', 'D4', 'C3', 'P8', 'C10')) %>% 
  select(-Latitude, -Longitude, -sal, -tn) %>% 
  group_by(Site_Code)

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
  mutate(location = 'Delta') %>% 
  data.frame
corests$location[corests$Site_Code %in% c('D7', 'D6', 'D4')] <- 'Suisun'

ggplot(corests, aes(x = lag, y = acf, colour = var2, group = var2)) +
  facet_grid(location + Site_Code ~ var1) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous('lag (months)') +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())



# site <- 'C10'
# flos <- 'sjr'
# 
# # filter the flow data by the relevation station, take log of Q, do left mwa for flow
# floavg <- filter(flow_dat, station == flos) %>% 
#   mutate(
#     logq = log(1 + q), 
#     logqavg = stats::filter(logq, sides = 1, filter = rep(1, 31)/31, method = 'convolution')
#     ) %>% 
#   select(Date, logqavg)
# 
# # data prep
# tomod <- filter(delt_dat, Site_Code == site) %>% 
#   left_join(floavg, by = 'Date') %>% 
#   select(Date, no23, logqavg) %>% 
#   mutate(
#     lim = -1e6,
#     no23 = log(1 + no23)
#     ) %>% 
#   data.frame %>% 
#   tidal(., 
#     reslab = expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')')), 
#     flolab = expression(paste('ln-flow (standardized)'))
#   )
