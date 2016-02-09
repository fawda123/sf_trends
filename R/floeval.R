library(ggplot2)
library(dplyr)
library(WRTDStidal)
library(tidyr)
library(lubridate)

load(file = 'data/delt_dat.RData')
load(file = 'data/flow_dat.RData')

sites <- unique(delt_dat$Site_Code)
flos <- unique(flow_dat$station) 
grd <- expand.grid(sites, flos)
names(grd) <- c('site', 'flos')

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

# format for plot
flocor <- do.call('rbind', outests) %>% 
  separate(nm, c('site', 'flo'), sep = '_') %>% 
  mutate(lag = factor(lag))
row.names(flocor) <- 1:nrow(flocor)

ggplot(flocor, aes(x = lag, y = acf, colour = flo, group = flo)) +
  geom_line() + 
  geom_point() + 
  theme_bw() +
  facet_wrap(~site)

# get the station, lag, and min cor for each site
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

delt_dat <- left_join(delt_dat, siteflo, by = c('Site_Code', 'Date'))
save(delt_dat, file = 'data/delt_dat.RData')
toplo <- select(delt_dat, Site_Code, logq, logqavg, no23) %>% 
  gather('var', 'val', logq:logqavg)
ggplot(toplo, aes(x = val, y = log(1 + no23), group = var, colour = var)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm') + 
  facet_wrap(~Site_Code, scales = 'free') + 
  theme_bw()
