library(WRTDStidal)
library(dplyr)
library(foreach)
library(doParallel)
     
#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)

data(delt_dat)
data(mods_opt)

# iterate through each site to get optimal window values and create mod
sites <- unique(delt_dat$Site_Code)
mods <- vector('list', length = length(sites))
names(mods) <- sites
mods <- foreach(site = sites, .packages = c('dplyr', 'WRTDStidal')) %dopar% {
  
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
  
  # get optimal window half-widhs, create mod
  wins_in <- as.list(mods_opt[[site]]$par)
  mod <- modfit(tomod, tau = c(0.1, 0.5, 0.9), wins = wins_in)

  # output
  mod
  
}
