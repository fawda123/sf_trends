library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(WRTDStidal)

data(tomod)

lablk <- list(
  shrt = c('din', 'nh', 'no23'),
  lngs = c(
    expression(paste('ln-dissolved inorganic nitrogen (mg ', L^-1, ')')),
    expression(paste('ln-ammonium (mg ', L^-1, ')')),
    expression(paste('ln-nitrite/nitrate (mg ', L^-1, ')'))
    )
  )

# weight reg by group
mods <- mutate(tomod, resdup = resvar) %>% 
  group_by(Location, Site_Code, resvar, flovar) %>% 
  nest %>% 
  mutate(mod = map(data, 
    function(x){
      
      # get label from resdup column
      reslab <- with(lablk, lngs[shrt == unique(x$resdup)])
      
      # create tidal object to model
      tmp <- select(x, -resdup) %>% 
        rename(
          res = resval, 
          flo = flolag
        ) %>% 
        data.frame %>% 
        tidal(., 
          reslab = reslab, 
          flolab = expression(paste('ln-flow (standardized)'))
        )
      
      # create model and exit
      out <- modfit(tmp, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 20, 0.5), flo_div = 30, min_obs = NULL)
      return(out)
      
    })
    
  )

save(mods, file = 'data/mods.RData', compress = 'xz')

