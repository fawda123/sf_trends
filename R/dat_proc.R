######
# data processing for discrete delta stations

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

######
# import ESRI shapefile, save as RData object
# shapefile from dissolved/clipped object created in ArcMap
# source data from http://www.sfei.org/data/california-aquatic-resource-inventory-cari-version-01-gis-data#sthash.ykwo9qLo.dpbs

delt_map <- maptools::readShapeSpatial('M:/GIS/sf_delta.shp')

save(delt_map, file = 'data/delt_map.RData', compress = 'xz')
save(delt_map, file = 'M:/docs/manuscripts/sftrends_manu/data/delt_map.RData', compress = 'xz')

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
  select(Site_Code, Date, Latitude, Longitude, nh, no23, din, tss, chl, sio2, tp, sal)

# finally, subset by selected delta and suisun stations
# make sure each date is unique
dat <- filter(dat, Site_Code %in% c('C3', 'C10', 'P8', 'D4', 'D7', 'D6', 'D19', 'D26', 'D28A')) %>% 
  mutate(
    year = year(Date), 
    month = month(Date), 
    day = day(Date)
  ) %>% 
  filter(year > 1975 & year < 2015) %>% 
  gather('var', 'val', Latitude:sal) %>% 
  group_by(Site_Code, year, month, day, var) %>% 
  summarize(val = mean(val, na.rm = TRUE)) %>% 
  spread(var, val) %>% 
  ungroup %>% 
  mutate(
    Date = as.Date(paste(year, month, day, sep = '-'), format = '%Y-%m-%d'),
    Location = 'Delta', 
    Site_Code = gsub('[A-Z]$', '', Site_Code)
    ) %>% 
  select(Site_Code, Location, Date, Latitude, Longitude, nh, no23, din, tss, chl, sio2, tp, sal)
dat$Location[dat$Site_Code %in%  c('D4', 'D7', 'D6')] <- 'Suisun'
dat$Location[dat$Site_Code %in% c('D19', 'D26', 'D28')] <- 'Middle'

delt_dat <- dat
save(delt_dat, file = 'data/delt_dat.RData')
save(delt_dat, file = 'M:/docs/manuscripts/sftrends_manu/data/delt_dat.RData', compress = 'xz')

######
# check outliers - none
# delt data have not been transformed, values below are from viz eval of ggpairs plots that were transformed

# data(delt_dat)
# data(flow_dat)
# # check for outliers
# toplo <- tidyr::spread(flow_dat, station, q) %>% 
#   select(-east) %>% 
#   left_join(delt_dat, ., by = 'Date') %>% 
#   select(-Latitude, -Longitude) %>% 
#   mutate(
#     tmps = 1:nrow(.)
#   ) %>% 
#   gather('var', 'val', -Site_Code, -Date, -Location, -tmps) %>% 
#   na.omit %>% 
#   mutate(
#     val = log(1 + val)
#     ) %>% 
#   spread(var, val)
# 
# vars <- c('chl', 'din', 'nh', 'no23', 'sac', 'sal', 'sio2', 'sjr', 'tp', 'tss')
# pdf('C:/Users/mbeck/Desktop/tmp.pdf', height = 9, width = 13, family = 'serif')
# for(vr in vars){
#   
#   p <- ggplot(toplo, aes_string(x = 'Date', y = vr)) + 
#     geom_point() + 
#     facet_wrap(~Site_Code, ncol = 1) +
#     theme_bw()
#   print(p)
# }
# dev.off()

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
    sac = sac
  ) %>% 
  select(Date, sac, east, sjr) %>% 
  gather('station', 'q', sac:sjr)

save(flow_dat, file = 'data/flow_dat.RData')
save(flow_dat, file = 'M:/docs/manuscripts/sftrends_manu/data/flow_dat.RData', compress = 'xz')

######
# dataset for wrtds, all response, response variables at each station matched to corresponding flow values in flomtch

rm(list = ls())

data(flow_dat)
data(delt_dat)

flomtch <- data.frame(
  Site_Code = c('C10', 'C3', 'P8', 'D4', 'D6', 'D7', 'D19', 'D26', 'D28'), 
  flovar = c('sjr', 'sac', 'sjr', 'sal', 'sal', 'sal', 'sac', 'sac', 'sac'), 
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

# import each file, add to nested mods_nolag dataframe
fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7|^D19|^D26|^D28', full.names = T)
dat <- lapply(fls, load, .GlobalEnv)
names(dat) <- unlist(dat)
dat <- lapply(dat, get)

mods <- unite(mods, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(data = dat[match(tmp, names(dat))]) %>% 
  select(-tmp) %>% 
  filter(resvar %in% c('din', 'nh', 'no23'))

# # remove the individual files
# file.remove(fls)

# save output, also to shiny app folder
save(mods, file = 'data/mods.RData', compress = 'xz')
save(mods, file = 'M:/docs/manuscripts/sftrends_manu/data/mods.RData', compress = 'xz')  

######
# trend summary for stations

rm(list = ls())

data(mods)

mobrks <- list(c(3, 4, 5), c(6, 7, 8), c(9, 10, 11), c(12, 1, 2))
yrbrks <- c(-Inf, 1995, Inf)
molabs <- c('Spring', 'Summer', 'Fall', 'Winter')
yrlabs <- c('1976-1995', '1996-2014')

# % changes
trnds_chg <- mutate(mods, 
  trnd = map(data, function(x){
    wrtdstrnd(x, mobrks, yrbrks, molabs, yrlabs, aves = T)
    })
  ) %>% 
  select(-data) %>% 
  unnest %>% 
  data.frame %>% 
  select(-flovar, -Location, -ave) %>% 
  spread(cat, chg) %>% 
  arrange(resvar)

save(trnds_chg, file = 'data/trnds_chg.RData')
save(trnds_chg, file = 'M:/docs/manuscripts/sftrends_manu/data/trnds_chg.RData', compress = 'xz')

# by averages
trnds_ave <- mutate(mods, 
  trnd = map(data, function(x){
    wrtdstrnd(x, mobrks, yrbrks, molabs, yrlabs, aves = T)
    })
  ) %>% 
  select(-data) %>% 
  unnest %>% 
  data.frame %>% 
  select(-flovar, -Location, -chg) %>% 
  spread(cat, ave) %>% 
  arrange(resvar)

save(trnds_ave, file = 'data/trnds_ave.RData')
save(trnds_ave, file = 'M:/docs/manuscripts/sftrends_manu/data/trnds_ave.RData', compress = 'xz')

# by sk
trnds_sk <- mutate(mods, 
  trnd = map(data, function(x){
    wrtdstrnd_sk(x, mobrks, yrbrks, molabs, yrlabs)
    })
  ) %>% 
  select(-data) %>% 
  unnest %>% 
  data.frame

save(trnds_sk, file = 'data/trnds_sk.RData')
save(trnds_sk, file = 'M:/docs/manuscripts/sftrends_manu/data/trnds_sk.RData', compress = 'xz')

######
# sk trends on predicted and flow-norm, seasonal aggs by year

data(mods)

##
# seasons within each year group
seasbyyr <- function(mods, trndvar = 'bt_norm'){
  
  mobrks <- list(c(3, 4, 5), c(6, 7, 8), c(9, 10, 11), c(12, 1, 2))
  yrbrks <- c(-Inf, 1995, Inf)
  molabs <- c('Spring', 'Summer', 'Fall', 'Winter')
  yrlabs <- c('1976-1995', '1996-2014')
  
  # norm trnds
  trnds <- mutate(mods, 
    trnd = map(data, function(x){
      
      bef <- x[x$year <= 1995, ] %>% 
        wrtdstrnd_sk(., mobrks, yrbrks, molabs, yrlabs, trndvar = trndvar) %>% 
        mutate(ann = 'bef')
    
      aft <- x[x$year > 1995, ]  %>% 
        wrtdstrnd_sk(., mobrks, yrbrks, molabs, yrlabs, trndvar = trndvar) %>% 
        mutate(ann = 'aft')
    
      rbind(bef, aft)
      
      })
    ) %>% 
    select(-data) %>% 
    unnest %>% 
    mutate(
      trndvar = gsub('bt_', '', trndvar)
    ) %>% 
    data.frame
  
  return(trnds)
  
}

trnds_nrm <- seasbyyr(mods, trndvar = 'bt_norm')
trnds_fit <- seasbyyr(mods, trndvar = 'bt_fits')
trnds_obs <- seasbyyr(mods, trndvar = 'res')

# combine all
trnds_seasyr <- rbind(trnds_nrm, trnds_fit, trnds_obs) %>% 
  select(Site_Code, resvar, cat, perchg, ann, trndvar) %>% 
  spread(trndvar, perchg) %>% 
  mutate(Site_Code = factor(Site_Code, levels = c('D7', 'D6', 'D4', 'D28', 'D26', 'D19', 'P8', 'C10', 'C3')))

save(trnds_seasyr, file = 'data/trnds_seasyr.RData', compress = 'xz')
save(trnds_seasyr, file = 'M:/docs/manuscripts/sftrends_manu/data/trnds_seasyr.RData', compress = 'xz')

######
# processing clam data at D7 from Craduer et al. 2016 report

# text files, taken manually from pdf conversion to text
fls <- c('ignore/CrauderApp9.txt', 'ignore/CrauderApp10.txt')

# function to process data from text files
#
# fl is text file name
# spp is chr string of species in the file
# mo_strt is the starting month for the year, defaults to October for USGS water year
form_dat <- function(fl, spp, mo_strt = 10){ 
  
  # read file
  tmp <- readLines(fl) %>% 
    grep('^$|^\\s*', ., invert = T, value = T)
  
  # put in wide format
  heads <- tmp[1:16]
  tmp <- tmp[!tmp %in% heads] %>% 
    data.frame(col = rep(1:8, length.out = length(.)), row = rep(1:(length(.)/8), each = 8), val = .) %>% 
    spread(col, val) %>% 
    select(-row)
  
  # addl formatting, add species
  names(tmp) <- c('date', 'clams_smp', 'biomass', 'recruit_area', 'mean_size', 'gr', 'depth', 'no_grabs')
  tmp <- gather(tmp, 'var', 'val', -date) %>% 
    mutate(val = as.numeric(val)) %>% 
    spread(var, val) %>% 
    mutate(
      date = as.Date(as.character(date), format = '%m/%d/%Y'),
      mo = month(date),
      yr = year(date), 
      yr = ifelse(mo < mo_strt, yr, yr + 1)
      ) %>%  
    arrange(date) %>% 
    mutate(species = spp) %>% 
    select(-mo)
  
  return(tmp)
  
  }

d7_corb <- form_dat(fls[1], 'Corbicula')
d7_pota <- form_dat(fls[2], 'Potamocorbula')

clams <- rbind(d7_corb, d7_pota)
save(clams, file = 'data/clams.RData')
save(clams, file = 'M:/docs/manuscripts/sftrends_manu/data/clams.RData', compress = 'xz')

######
# potw loads from Tracy and Stockton
# see Bresnahan email 6/20
# note that TN is probably wrong in original data, recalculated here

stockton <- read.csv('ignore/Stockton POTW.csv') %>% 
  mutate(
    date = as.Date(X, format = '%d-%b-%y'), 
    loc = 'stock', 
    TN = DIN
    ) %>% 
  select(-X, -DIN) %>% 
  gather('var', 'val', NH4:TN) %>% 
  mutate(var = tolower(var))

stock_load <- stockton

save(stock_load, file = 'data/stock_load.RData', compress = 'xz')
save(stock_load, file = 'M:/docs/manuscripts/sftrends_manu/data/stock_load.RData', compress = 'xz')

######
# stockton effluent concentrations
# see Jabusch email 11/2/16

# import all
stockton <- read_excel('ignore/COS Receiving Water 1992-09 through 2009-03.xls') %>% 
  as.data.frame(., stringsAsFactors = FALSE)

# rows, columns to keep
rowsel <- grep('STOCKTON WWTP', stockton[, 2]) %>% 
  c(., which(is.na(stockton[, 2]))) %>% 
  sort
colsel <- paste0(c('NH3', 'Nitrite', 'Nitrate')) %>% 
  paste(., collapse = '|') %>% 
  grep(. , names(stockton)) %>% 
  c(1, 2, .)

# subset by rows, columns, fill date, rename variables
# Thomas said that R1 is upstream of plant
dat <- stockton[-rowsel, colsel]
names(dat)[is.na(names(dat))] <- 'date'
names(dat)[grep('STOCKTON', names(dat))] <- 'dy'
dat <- mutate(dat,
    yr = year(date), 
    yr = zoo::na.locf(yr, na.rm = F),
    mo = month(date),
    mo = zoo::na.locf(mo, na.rm = F),
    dy = gsub('(^[0-9]*).*', '\\1', dy)
  ) %>% 
  select(-date) %>% 
  unite('date', yr, mo, dy, sep = '-') %>% 
  mutate(date = as.Date(date)) %>% 
  gather('var', 'val', -date) %>% 
  extract(var, c('site', 'var'), regex = '(^R[0-9].*) (\\n.*\\n.*)') %>% 
  filter(!grepl('R[1]', site)) %>% 
  mutate(
    var = gsub('.*(NH[3]).*', '\\1', var),
    var = gsub('.*(NO[2]).*', '\\1', var),
    var = gsub('.*(NO[3]).*', '\\1', var),
    val = as.numeric(val)
  ) %>% 
  group_by(var) %>% 
  mutate(val = zoo::na.approx(val, na.rm = F)) %>% 
  group_by(date, var) %>% 
  summarize(val = sum(val)) %>% 
  ungroup %>% 
  arrange(var, date) %>% 
  group_by(var) %>% 
  mutate(
    val2 = stats::filter(val, rep(1/20, 20), sides = 2), 
    val2 = as.numeric(val2)
    ) %>% 
  filter(date >= as.Date('2002-01-01')) %>%
  data.frame
# 
# ggplot(dat, aes(x = date, y = val, colour = var)) +
#   geom_line() +
#   # geom_line(aes(y = val2), linetype = 'dashed') +
#   theme_minimal()

stock_conc <- dat
save(stock_conc, file = 'data/stock_conc.RData', compress = 'xz')
save(stock_conc, file = 'M:/docs/manuscripts/sftrends_manu/data/stock_conc.RData', compress = 'xz')

######
# create quantile models for first hypothesis in paper

rm(list = ls())

data(mods)

# filter c10
h1dat <- filter(mods, Site_Code %in% 'C10')

# save output
save(h1dat, file = 'data/h1dat.RData', compress = 'xz')
save(h1dat, file = 'M:/docs/manuscripts/sftrends_manu/data/h1dat.RData', compress = 'xz')

######
# create mean models for second hypothesis in paper
# wrtds mean models for chl, din, nh, no23, and sio2 at P8

rm(list = ls())

# import each file, add to nested dat dataframe
fls <- list.files('data', pattern = '^P8', full.names = T) %>% 
  grep('chl|din|no23|nh|sio2', ., value = T)
moddat <- lapply(fls, load, .GlobalEnv)
names(moddat) <- unlist(moddat)
moddat <- lapply(moddat, get)

Location <- 'Delta'
Site_Code <- 'P8'
resvar <- c('chl', 'din', 'nh', 'no23', 'sio2')
flovar <- 'sjr'
h2dat <- expand.grid(Location, Site_Code, resvar, flovar)
names(h2dat) <- c('Location', 'Site_Code', 'resvar', 'flovar')
h2dat$data <- NA
h2dat <- nest(h2dat, data)

# add model data to nested data frame
h2dat <- unite(h2dat, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(data = moddat[match(tmp, names(moddat))]) %>% 
  select(-tmp)

# save output
save(h2dat, file = 'data/h2dat.RData', compress = 'xz')
save(h2dat, file = 'M:/docs/manuscripts/sftrends_manu/data/h2dat.RData', compress = 'xz')

######
# create mean models for third hypothesis in paper
# wrtds mean models for chl, din, sio2 at c10 and d7

rm(list = ls())

# import each file, add to nested dat dataframe
fls <- list.files('data', pattern = '^D7|C10', full.names = T) %>% 
  grep('chl|din|sio2', ., value = T)
moddat <- lapply(fls, load, .GlobalEnv)
names(moddat) <- unlist(moddat)
moddat <- lapply(moddat, get)

Location <- 'Delta'
Site_Code <- c('D7', 'C10')
resvar <- c('chl', 'din', 'sio2')
flovar <- 'sjr'
h3dat <- expand.grid(Location, Site_Code, resvar, flovar)
names(h3dat) <- c('Location', 'Site_Code', 'resvar', 'flovar')
h3dat$data <- NA
h3dat <- nest(h3dat, data)

# add model data to nested data frame
h3dat <- unite(h3dat, 'tmp', Site_Code, resvar, remove = F) %>% 
  mutate(data = moddat[match(tmp, names(moddat))]) %>% 
  select(-tmp)

# save output
save(h3dat, file = 'data/h3dat.RData', compress = 'xz')
save(h3dat, file = 'M:/docs/manuscripts/sftrends_manu/data/h3dat.RData', compress = 'xz')

######
# remove the invidual model files, these are too big for commits
# do this last, the above stuff calls these files, they have also been uploaded to S3
# mods only has nitrogen

fls <- list.files('data', pattern = '^C3|^C10|^P8|^D6|^D4|^D7|^D19|^D26|^D28', full.names = T)
file.remove(fls)

