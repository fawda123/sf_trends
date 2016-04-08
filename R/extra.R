######
# load libraries and data

library(readxl)
library(dplyr)
library(ggplot2)
library(WRTDStidal)
library(maptools)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(scales)

data(delt_dat)
data(delt_map)
data(mods_nolag)

##
# get a bounding box for the stations
statmeta <- select(delt_dat, Site_Code, Latitude, Longitude) %>% 
  unique

# bounding box
lims<- select(statmeta, -Site_Code) %>% 
  apply(., 2, function(x) range(x, na.rm = TRUE))
buff <- 0.002
lims <- c(
  (1 + buff) * lims[1, 2], 
  (1 - buff) * lims[1, 1], 
  (1 - buff) * lims[2, 2], 
  (1 + buff) * lims[2, 1]
  )    
names(lims) <- c('left', 'bottom', 'right', 'top')

##
# get trends using wrtdstrnd function

# args to split
mobrks <- c(-Inf, 3, 6, 9, Inf)
yrbrks <- c(-Inf, 1985, 1994, 2003, Inf)
molabs <- c('JFM', 'AMJ', 'JAS', 'OND')
yrlabs <- c('1976-1985', '1986-1994', '1995-2003', '2004-2012')

# get trends, merge with statmeta for lat/lon
trnds <- mutate(mods_nolag, 
  trnd = map(mod, function(x){
    wrtdstrnd(x, mobrks, yrbrks, molabs, yrlabs, tau = 0.5)
    })
  ) %>% 
  select(-data, -mod) %>% 
  unnest %>% 
  left_join(., statmeta, by = 'Site_Code') %>% 
  data.frame

##
# create some plots

# convert delta map to ggplot compatible object
delt_map <- fortify(delt_map)

# base delta map
pbase <- ggplot(delt_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = hole)) +
  scale_fill_manual(values=c("lightblue", "#FFFFFF"), guide="none") +  
  theme(axis.line=element_blank(), axis.text.x=element_blank(),
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank(), 
    legend.position = 'top', 
    legend.title = element_blank()
    ) +  
  coord_fixed(ratio = 1, xlim = lims[c(1, 3)], ylim = lims[c(2, 4)])

# globals for plot and legend
cols <- brewer.pal(9, 'Set1') %>% 
  .[c(1, 3)]
sz_rng <- c(0.5, 7.5)
shps <- c(24, 25)
res <- 'no23'

# some formatting for the variable to plot
toplo <- filter(trnds, resvar == res)
toplo$shp <- shps[1]
toplo[toplo$chg < 0, 'shp'] <- shps[2]
toplo$sz <- rescale(abs(toplo$chg), to = sz_rng)
toplo$col <- cols[1]
toplo[toplo$chg < 0, 'col'] <- cols[2]

pbase +
  geom_point(data = toplo, 
    aes(x = Longitude, y = Latitude), 
    pch = toplo$shp, size = toplo$sz, fill = toplo$col, alpha = 0.8)  + 
  facet_wrap( ~ cat, ncol = 4)

## manual changes to legend
ndivs <- 6

legtitle <- 'Percent change'
legcols <- rep(cols, each = ndivs/2)
legshps <- rep(shps, each = ndivs/2)

# sizes 
negs <- with(toplo, sign(chg) == -1)
dec <- toplo$sz[negs]
dec <- seq(min(dec), max(dec), length = ndivs/2)
inc <- toplo$sz[!negs]
inc <- seq(min(inc), max(inc), length = ndivs/2)
legszs <- rev(c(rev(dec), inc))

# labels
dec <- toplo$chg[negs]
dec <- seq(min(dec), max(dec), length = ndivs/2)
inc <- toplo$chg[!negs]
inc <- seq(min(inc), max(inc), length = ndivs/2)
leglab <- round(c(dec, inc), 1)


fakeleg <- data.frame(
  shp = leglab,
  sz = leglab,
  col = leglab
)

ggplot(fakeleg, aes(x = shp, y = sz, fill = factor(col), size = factor(sz), shape = factor(shp))) +
  geom_point() +
  theme_minimal() + 
  theme(legend.position = 'right') +
  guides(
    col = guide_legend(title = legtitle, nrow = 2), 
    size = guide_legend(title = legtitle, nrow = 2), 
    shape = guide_legend(title = legtitle, nrow = 2), nrow = 1
    ) +
  scale_fill_manual(legtitle, values = legcols) +
  scale_size_manual(legtitle, values = legszs) +
  scale_shape_manual(legtitle, values = legshps)

  