# library(readxl)
# library(dplyr)
# library(ggmap)
# 
# data(delt_dat)
# 
# statmeta <- select(delt_dat, Site_Code, Latitude, Longitude) %>% 
#   unique
# 
# # bounding box
# locs <- select(statmeta, -Site_Code) %>% 
#   apply(., 2, function(x) range(x, na.rm = TRUE))
# buff <- 0.002
# locs <- c(
#   (1 + buff) * locs[1, 2], 
#   (1 - buff) * locs[1, 1], 
#   (1 - buff) * locs[2, 2], 
#   (1 + buff) * locs[2, 1]
#   )    
# names(locs) <- c('left', 'bottom', 'right', 'top')
# 
# map <- get_map(
#   location = locs, 
#   maptype = 'terrain-background', 
#   source = 'stamen'
#   ) 
# 
# ggmap(map) + 
#   theme(
#     axis.title = element_blank()
#   ) + 
#   geom_point(data = statmeta, aes(x = Longitude, y = Latitude), size = 4)

library(lubridate)
library(dplyr)

data(mods_nolag)

tmp <- mods_nolag$mod[[1]]

quant <- 0.5
mo_cats <- 4
yr_cats <- 4
  
# columns to get with regex
toget <- c('^date$', paste0('^norm', quant, '$'))
toget <- paste(toget, collapse = '|')

toeval <- select(tmp, matches(toget)) %>% 
  mutate(
    

# combine stations, agg by year for slope ests
alldat <- rbind(
  data.frame(bestLE12[, !names(bestLE12) %in% 'lnQ'], stat = 'LE12', stringsAsFactors = F), 
  data.frame(bestTF16[, !names(bestTF16) %in% 'sal'], stat = 'TF16', stringsAsFactors = F)
  ) %>% 
  select(stat, date, norm_wrtds, norm_gams, mocat, yrcat, flcat) %>% 
  mutate(
    date = as.numeric(strftime(date, '%Y'))
  )
  
# these need to be aggregated separately

# annual aggs
yrdat <- group_by(alldat, stat, date, yrcat) %>% 
  summarise(
    norm_wrtds = mean(norm_wrtds, na.rm = T), 
    norm_gams = mean(norm_gams, na.rm = T)
  ) 

# monthly aggs
modat <- group_by(alldat, stat, date, mocat) %>% 
  summarise(
    norm_wrtds = mean(norm_wrtds, na.rm = T), 
    norm_gams = mean(norm_gams, na.rm = T)
  )

# flo aggs
fldat <- group_by(alldat, stat, date, flcat) %>% 
  summarise(
    norm_wrtds = mean(norm_wrtds, na.rm = T), 
    norm_gams = mean(norm_gams, na.rm = T)
  )

# get summary stats for each agg period

# percent change across period
# based on differences between average of earliest three and latest three years
# to minimize outliers
# use single = T to use only one year for first and last comparisons
chn_fun <- function(x, y, single = F){

  tomod <- data.frame(x, y)
  tomod <- tomod[order(tomod$x), ]
  
  # get only first and last year's data
  if(single){
  
    srt <- tomod[which.min(tomod$x), 'y']
    stp <- tomod[which.max(tomod$x), 'y']
  
  # otherwise get average of first and last three years of data
  } else {
    
    srt <- which.min(tomod$x)
    srt <- seq(srt, srt + 2)
    srt <- mean(tomod[srt, 'y'], na.rm = TRUE)
    stp <- which.max(tomod$x)
    stp <- seq(stp - 2, stp)
    stp <- mean(tomod[stp, 'y'], na.rm = TRUE)
      
  }
  
  # get percent change from beginning to end
  out <- 100 * (stp - srt)/srt
  return(out)
  
}

yrdat <- gather(yrdat, 'mod', 'val', norm_wrtds:norm_gams) %>% 
  mutate(mod = gsub('^norm_', '', mod)) %>% 
  group_by(stat, yrcat, mod) %>% 
  summarise(
    ave = mean(val, na.rm = TRUE),
    chg = chn_fun(date, val, single = T) # important!
  ) %>% 
  gather('met', 'val', ave:chg) %>% 
  unite('modmet', mod, met) %>% 
  spread(modmet, val) %>% 
  rename(cat = yrcat)

modat <- gather(modat, 'mod', 'val', norm_wrtds:norm_gams) %>% 
  mutate(mod = gsub('^norm_', '', mod)) %>% 
  group_by(stat, mocat, mod) %>% 
  summarise(
    ave = mean(val, na.rm = TRUE),
    chg = chn_fun(date, val)
  ) %>% 
  gather('met', 'val', ave:chg) %>% 
  unite('modmet', mod, met) %>% 
  spread(modmet, val) %>% 
  rename(cat = mocat)

fldat <- gather(fldat, 'mod', 'val', norm_wrtds:norm_gams) %>% 
  mutate(mod = gsub('^norm_', '', mod)) %>% 
  group_by(stat, flcat, mod) %>% 
  summarise(
    ave = mean(val, na.rm = TRUE),
    chg = chn_fun(date, val)
  ) %>% 
  gather('met', 'val', ave:chg) %>% 
  unite('modmet', mod, met) %>% 
  spread(modmet, val) %>% 
  rename(cat = flcat)

# summary across all years
allsum <- gather(alldat, 'mod', 'val', norm_wrtds:norm_gams) %>% 
  mutate(mod = gsub('^norm_', '', mod)) %>% 
  group_by(stat, date, mod) %>% 
  summarise(
    val = mean(val, na.rm = TRUE)
  ) %>% 
  group_by(stat, mod) %>% 
  summarise(
    ave = mean(val, na.rm = TRUE),
    chg = chn_fun(date, val)
  ) %>% 
  gather('met', 'val', ave:chg) %>% 
  unite('modmet', mod, met) %>% 
  spread(modmet, val) %>% 
  mutate(cat = '') %>% 
  select(stat, cat, gams_ave, gams_chg, wrtds_ave, wrtds_chg)

totab <- rbind(allsum, yrdat, modat, fldat) %>% 
  ungroup %>% 
  mutate(cat = factor(cat, 
    levels = c('', '1986-1993', '1994-2000', '2001-2007', '2008-2014', 'JFM', 'AMJ', 'JAS', 'OND', 'Flow 1 (Low)', 'Flow 2', 'Flow 3', 'Flow 4 (High)'), 
    labels = c('', '1986-1993', '1994-2000', '2001-2007', '2008-2014', 'JFM', 'AMJ', 'JAS', 'OND', '1 (Low)', '2', '3', '4 (High)')
    )) %>% 
  arrange(stat, cat) %>% 
  data.frame(.)

# for inline expressions
trendsLE12 <- totab[totab$stat %in% 'LE12', ]
trendsTF16 <- totab[totab$stat %in% 'TF16', ]
save(trendsLE12, file = 'data/trendsLE12.RData')
save(trendsTF16, file = 'data/trendsTF16.RData')




