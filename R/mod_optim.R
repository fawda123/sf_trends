library(WRTDStidal)
library(dplyr)

data(delt_dat)

library(doParallel)
ncores <- detectCores() - 2  
registerDoParallel(cores = ncores)

sites <- c('C3', 'D28A', 'P8') #unique(delt_dat$Site_Code)
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
  
#   mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9)) %>% 
#     respred %>% 
#     resnorm
  
  mod <- winsrch_optim(tomod, tau = 0.5)
  
  mods_opt[[site]] <- mod

  save(mods_opt, file = 'data/mods_opt.RData')
  
}

save(mods_opt, file = 'data/mods_opt.RData')
    
  
  