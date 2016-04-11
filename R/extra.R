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
library(gridExtra)
library(ggrepel)
source('R/funcs.R')

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
buffy <- 0.001
buffx <- 0.00025
lims <- c(
  (1 + buffx) * lims[1, 2], 
  (1 - buffy) * lims[1, 1], 
  (1 - buffx) * lims[2, 2], 
  (1 + buffy) * lims[2, 1]
  )    
names(lims) <- c('left', 'bottom', 'right', 'top')

##
# get trends using wrtdstrnd function

# args to split
mobrks <- c(-Inf, 4, 8, Inf)
yrbrks <- c(-Inf, 1988, 2000, Inf)
molabs <- c('JFMA', 'MJJA', 'SOND')
yrlabs <- c('1976-1988', '1989-2000', '2001-2012')

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
  theme_bw() +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank(), 
    legend.position = 'none'
    ) +  
  coord_fixed(ratio = 1, xlim = lims[c(1, 3)], ylim = lims[c(2, 4)])

# globals for plot and legend
cols <- brewer.pal(9, 'Set1') %>% 
  .[c(1, 3)]
sz_rng <- c(2, 11)
shps <- c(24, 25)
res <- 'no23'

# some formatting for the variable to plot
toplo <- filter(trnds, resvar == res)
toplo$shp <- shps[1]
toplo[toplo$chg < 0, 'shp'] <- shps[2]
toplo$shp <- factor(toplo$shp)
toplo$sz <- rescale(abs(toplo$chg), to = sz_rng)
toplo[!toplo$cat %in% yrlabs[1] , 'Site_Code'] <- NA

# barplots
toplobr <- filter(trnds, resvar == res)
toplobr$cat_grp <- 'yr'
toplobr$cat_grp[grepl('[A-Z]', toplobr$cat)] <- 'mo'
toplobr$cat_grp <- factor(toplobr$cat_grp, levels  = c('yr', 'mo'))

ggplot(toplobr, aes(x = Site_Code, y = chg, fill = cat)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_wrap(~ cat_grp, ncol = 1) +
  theme_bw()

ptrnd <- pbase +
  geom_label_repel(
    data = toplo, 
    aes(x = Longitude, y = Latitude, label = Site_Code), 
    label.r = unit(0, "lines"),
    box.padding = unit(1, "lines"), 
    point.padding = unit(0, "lines"), 
    force = 2, size = 2,
    fill = 'lightgrey'
  ) +
  geom_point(data = toplo, 
    aes(x = Longitude, y = Latitude, size = sz, fill = shp, shape = shp),
    alpha = 0.8, size = toplo$sz
  )  + 
  scale_shape_manual(values = shps) + 
  scale_size(range = sz_rng) + 
  scale_fill_manual(values=c(cols, "cornflowerblue", "#FFFFFF"), guide="none") +
  facet_wrap( ~ cat, ncol = 3)

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

fakedat<- data.frame(
  shp = leglab,
  sz = leglab,
  col = leglab
)

pleg <- ggplot(fakedat, aes(x = shp, y = sz, fill = factor(col), size = factor(sz), shape = factor(shp))) +
  geom_point() +
  scale_fill_manual(legtitle, values = rev(legcols), labels = leglab) +
  scale_size_manual(legtitle, values = rev(legszs), labels = leglab) +
  scale_shape_manual(legtitle, values = rev(legshps), labels = leglab) +
  guides(
    fill = guide_legend(title = legtitle, nrow = 1), 
    size = guide_legend(title = legtitle), 
    shape = guide_legend(title = legtitle)
    ) +
  theme_minimal() + 
  theme(legend.position = 'top')

pleg <- g_legend(pleg)


pdf('C:/Users/mbeck/Desktop/test.pdf', height = 6, width = 9, family = 'serif')
grid.arrange(
  arrangeGrob(
    pleg, ptrnd, ncol = 1, 
    heights = c(0.12, 1))
)
dev.off()


  