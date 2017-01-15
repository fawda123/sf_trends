library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(purrr)
library(WRTDStidal)
library(foreach)
library(doParallel)
library(purrr)

rm(list = ls())

data(flow_dat)
data(delt_dat)

flomtch <- data.frame(
  Site_Code = c('C10', 'C3', 'MD10', 'P8', 'D4', 'D6', 'D7', 'D19', 'D26', 'D28'), 
  flovar = c('sjr', 'sac', 'sjr', 'sjr', 'sal', 'sal', 'sal', 'sac', 'sac', 'sac'), 
  stringsAsFactors = FALSE
)

flow_dat <- spread(flow_dat, station, q) %>% 
  filter(Date >= as.Date('1976-01-01') & Date <= as.Date('2014-12-31')) %>% 
  select(-east)

# process suisun stations separately, they use salinity not flow
suis <- filter(delt_dat, Location %in% 'Suisun') %>% 
  select(-Latitude, -Longitude) %>% 
  gather('resvar', 'resval', nh:tp) %>% 
  gather('flovar', 'floval', sal)

# middle and delta stations, flow records mathced by station using flomtch above
midsdelt <- filter(delt_dat, !Location %in% 'Suisun') %>% 
  mutate(Site_Code = as.character(Site_Code)) %>% 
  select(-Latitude, -Longitude, -sal) %>% 
  split(.$Site_Code) %>% 
  lapply(., function(x){
    
    flov <- unique(x$Site_Code) 
    flov <- with(flomtch, flovar[Site_Code %in% flov])
    flow_comb <- flow_dat[, c('Date', flov)]
    
    out <- left_join(flow_comb, x, by = 'Date') %>% 
      mutate(
        Site_Code = unique(na.omit(Site_Code)),
        Location = unique(na.omit(Location))
      )

    out <- gather(out, 'flovar', 'floval', matches(flov)) %>% 
      gather('resvar', 'resval', nh:tp)
    
    out
    
  }) %>% 
  do.call('rbind', .)

# combine suisun and midsdelt, nest
mods <- rbind(suis, midsdelt) %>% 
  arrange(Location, Site_Code) %>% 
  mutate(
    resdup = resvar, 
    flodup = flovar
  ) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest

##
# prep mods data for WRTDS

limlk <- data.frame(
  resvar = c('nh', 'no23', 'din', 'tss', 'chl', 'sio2', 'tp'), 
  limval = c(0.01, 0.01, 0.01, 1, 0.05, 0.01, 0.01)
)

# resvar label lookup
lablk <- list(
  shrt = c('din', 'nh', 'no23', 'chl', 'sio2', 'tss', 'tp'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')')),
    expression(paste('ln-chlorophyll a (ug ', L^-1, ')')),
    expression(paste('ln-silicon dioxide (mg ', L^-1, ')')),
    expression(paste('ln-total suspended solids (mg ', L^-1, ')')),
    expression(paste('ln-total phosphorus (mg ', L^-1, ')'))
    )
  )

# flovar label lookup
stalk <- list(
  shrt = c('sjr', 'sac', 'sal'),
  lngs = c('San Joaquin', 'Sacramento', 'Salinity')
  )

mods$data <- lapply(mods$data, function(x){
  
  # find the limit
  limv <- unique(x$resdup)
  limv <- with(limlk, limval[resvar == limv])
  
  # find the res label
  resv <- unique(x$resdup)
  resv <- with(lablk, lngs[shrt == resv])
  
  # find the flo label
  flov <- unique(x$flodup)
  flov <- with(stalk, lngs[shrt == flov])
  
  # log transform flo values, salinity has zeroes so plus 1
  if('sal' %in% x$flodup){
    
    x$floval <- log(1 + x$floval)
    
  } else {
   
    x$floval <- log(x$floval) 
    
  }

  # add limit, floor resval at limit, log of both
  x <- mutate(x,
      lim = limv,
      resval = pmax(lim, resval),
      lim = log(lim), 
      resval = log(resval)
    ) %>% 
    rename(
      res = resval, 
      flo = floval
    ) %>% 
    select(Date, res, flo, lim) %>% 
    data.frame %>% 
    tidalmean(., reslab = resv, flolab = flov)
      
  x

})  

##
# fit wrtds mods

mods <- filter(mods, Site_Code %in% 'MD10' & resvar %in% c('nh', 'no23', 'din'))

cl <- makeCluster(7)
registerDoParallel(cl)

strt <- Sys.time()

# iterate through stations, res vars to model
# get predictions from obs time series of salinity or flow
foreach(i = 1:nrow(mods)) %dopar% {
  
  library(WRTDStidal)
  
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(i, 'of', nrow(mods), '\n')
  print(Sys.time()-strt)
  sink()

  # fit model
  out <- modfit(mods$data[[i]], wins = list(0.5, 10, 0.5))

  # assign to unique object, save in case of fuckery
  resvar <- mods[i, ]$resvar
  sta <- mods[i, ]$Site_Code
  outnm <- paste0(sta, '_', resvar)
  assign(outnm, out)
  save(list = outnm, file = paste0('data/', outnm, '.RData'), compress = 'xz')
  
}