# README
Marcus W. Beck, beck.marcus@epa.gov  

Content available at [https://github.com/fawda123/sf_trends](https://github.com/fawda123/sf_trends)

### Files

**_data/_** Supporting RData files, usually from data in ignore folder, unless otherwise noted all files were created in `R/dat_proc.R`

* `clams.RData` clam data for D7 from Crauder 2016, includes sample date, water year (`yr` starting in October), biomass in g/m2, clams per sample (`clams_smp`) in no/m2, depth (m), growth rate (`gr`) in m3/m2/d, mean size (mm), number of grabs (`no_grabs`), recruitment per unit area (`recruit_area`) in recruit/0.05 m2, and species as corbicula or potamocorbula.

* `delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results

* `delt_map.RData` SpatialPolygonsDataFrame object of approximate map of Delta region

* `ignore/dwr_wq.RData` time series data of stations in the SF delta from California DWR-EMP (Department of Water Resources, Environmental Monitoring Program) , processed by E. Novick, all stations, analytes from 1975 to present.  Most analytes are measured as concentration, see original spreadsheet for values.  Unavailable in GitHub repo.

* `flow_dat.RData` time series of daily flow estimates for the delta (m3/s), input stations from Novick et al (Fig 2) were used

* `h1dat.RData` mean models to support first hypothesis/case study in manuscript, wrtds mean models for din, no23, nh at C10

* `h2dat.RData` mean models to support second hypothesis/case study in manuscript, wrtds mean models for no23, nh at p8

* `h3dat.RData` mean models to support third hypothesis/case study in manuscript, wrtds mean models for sio2, din, chla at c10, d7

* `mods.RData` dataset for wrtds, including model results. This is a nested data frame with identifiers, nitrogen only.

* `sf_bay.RData` SpatialPolygonsDataFrame object of all of SF Bay

* `stock_conc.RData` summarized effluent data from Stockton wwtp, mg/L

* `stock_load.RData` nutrient load data from Stockton, kg/day

* `trnds_ave.RData` trend summary of averages by annual, monthly categories

* `trnds_chg.RData` trend summary of percent changes by annual, monthly categories

* `trnds_sk.RData` trend summary using seasonal kendall on flow-normalized results

* `trnds_seasyr.RData` sk trends on predicted and flow-norm WRTDS results, seasonal aggs by year

**_R/_** Supporting R scripts

**_text/_** Summary text of analyses


```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(lubridate)
library(purrr)
library(GGally)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(WRTDStidal)
source('R/funcs.R')
```

### Model performance {.tabset}

#### RMSE

```r
# load the data
data(mods)

ylims <- c(0, 0.75)
ytext <- -0.04

## get model performance from the data objects

# perfs
perfs <- mutate(mods,
    rmse = map(data, function(x) wrtdsperf(x, logspace = F)[, 1]),
    nmse = map(data, function(x) wrtdsperf(x, logspace = F)[, 2])
  ) %>% 
  select(-data) %>% 
  unnest

# the plot
ggplot(perfs, aes(x = Site_Code, y = rmse)) +
  geom_bar(stat = 'identity') + 
  facet_grid(~resvar) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    legend.position = 'top',
    legend.title = element_blank(), 
    axis.title.x= element_blank()
    ) + 
  scale_y_continuous('RMSE', limits = ylims)
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### NMSE

```r
# the plot
ggplot(perfs, aes(x = Site_Code, y = nmse)) +
  geom_bar(stat = 'identity') + 
  facet_grid(~resvar) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    legend.position = 'top',
    legend.title = element_blank(), 
    axis.title.x= element_blank()
    ) + 
  scale_y_continuous('NMSE', limits = ylims)
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Annually-averaged results {.tabset}

The plots below show annually-averaged results of weighted regression for each station using the combined nutrient records and flow/salinity data.  The three lines in each plot represent model results for the conditional distributions of the 10th, 50th, and 90th percentiles of the nutrient record.  Points represent model predictions and lines are flow-normalized predictions that show trends independent of flow variation.The following describes points of interest that can be idenfied from the plots:

* General trends - flow-normalized trends over time are the most descriptive of changes, are nutrients decreasing, inreasing, or constant?
* Differences in the percentiles - variation in the 10th or 90th percentile distributions that differ from the median response suggest changes in frequency occurrence of low or high nutrient events, respectively.  In other words, the median response does not tell the whole picture about change in nutrient concentrations over time.  
* Differences in flow-normalized predictions and observed predictions - large differences between the two represent either a large effect of flow or an inappropriate flow variable
* Differences in the magnitude of the modelled response between locations - differences can show the relative proportions of nitrogen species at each site.  This is why the y-axis limits are constant for each response measure.
* Differences by nutrient species - how do the plots change with the nutrient species given any of the above information?  

See [here](https://beckmw.shinyapps.io/sf_trends/) for an interactive application of model results. 

#### DIN

```r
data(mods)

# y axis limits
lims <- data.frame(Delta = c(3, 0.85, 2.6), Middle = c(0.9, 0.175, 0.8), Suisun = c(1, 0.2, 0.8))
row.names(lims) <- c('din', 'nh', 'no23')

for(i in 1:nrow(mods)){

  toplo <- mods$data[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  # axis limits by resp var and location var
  resv <- as.character(mods[i, 'resvar'])
  locv <- as.character(mods[i, 'Location'])
  limy <- lims[resv, locv]
    
  p <- prdnrmplot(toplo, logspace = F, min_mo = 11) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(
      axis.title = element_blank(), 
      legend.position = 'top', 
      axis.line.x = element_line(size = 0.5),
      axis.line.y = element_line(size = 0.5) 
    ) +
    scale_y_continuous(limits = c(0, limy))

  # get legend
  if(i == 1) pleg <- g_legend(p)
  p <- p + theme(legend.position = 'none')

  assign(paste0('p', i), p)

}

# # for ref
# din <- which(mods$resvar == 'din')
# nh <- which(mods$resvar == 'nh')
# no23 <- which(mods$resvar == 'no23')

ylab1 <- gsub('ln-', '', as.character(attr(mods$data[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$data[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$data[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, p21, p24, p27, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### NH4

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, p19, p22, p25, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### NO23 

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, p20, p23, p26, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Flow by nutrients by time {.tabset}

The plots below show changes over time in the relationship between nutrients and flow.  The plots are also separated by month because changes are expected to vary by season. In general, flow increases (or salinity decreases) are related to decreases in nutrient concentrations as more freshwater inputs have a dilution effect. The data in each plot are from the interpolation grid for weighted regression that is created during model fitting.  The data are model predicions from the fit at each unique point in the time series.  The flow values that are observed in each month across all years are used for model fitting and prediction.  The following information can be obtained from each plot:

* General response of nutrients to flow - a negative relationship is of course common.
* Changes in the response over time - does the relationship vary from early to later in the time series?  These changes might suggest system response to different sources of pollution.  Previous studies (Hirsch et al. 2010, Beck and Hagy 2015) suggested that changes in this response might indicate shifts between point and non-point sources of pollution.  That is, nutrients will show minimal response to changes in flow (flat lines) if point-sources dominate load inputs because they will not vary with flow.  Conversely, non-point sources of pollution will change with flow such that a negative response of nutrient concention to increasing flow may suggest a larger influence of non-point sources. 
* Changes in the concentration over time independent of the flow response - are the response lines higher or lower from early to later in the time series?  This can indicate a change in the average concentration, which should also be verified with the plots above.
* Changes by season - from top to bottom (variation by rows), how does the nutrient-flow relatinship vary throughout the year?  How does the nutrient-flow relatonship vary by season and throughout the time series (i.e., season/year interaction)?  Seasonal changes may be expected given climate patterns and flow inputs throughout the year.  Seasonal changes may also occur based on the succession of phytoplankton species throughout the year and the affinity for different nutrients. 
* Changes between sites - how does the flow/nutrient response vary by location (variation between columns)?  This could differ for a number of reasons. 
* Changes by nutrient species - how do the plots change with the nutrient species given any of the above information?  

#### DIN

```r
data(mods)

# y axis limits
lims <- data.frame(Delta = c(3.2, 1.2, 3), Middle = c(1.5, 0.25, 1.4), Suisun = c(1, 0.25, 1))
row.names(lims) <- c('din', 'nh', 'no23')

for(i in 1:nrow(mods)){

  toplo <- mods$data[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  # axis limits by resp var and location var
  resv <- as.character(mods[i, 'resvar'])
  locv <- as.character(mods[i, 'Location'])
  limy <- lims[resv, locv]
    
  p <- dynaplot(toplo, month = c(1, 4, 7, 10), ncol = 4, logspace = F) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(
      axis.title = element_blank(), 
      legend.position = 'top', 
      axis.line.x = element_line(size = 0.5),
      axis.line.y = element_line(size = 0.5) 
      ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, limy))

  # flip if Suisun (salinity was used)
  if(as.character(mods[i, 'Location']) == 'Suisun')
    p <- p + scale_x_reverse()
  
  # get legend
  if(i == 1) pleg <- g_legend(p)
  p <- p + theme(legend.position = 'none')

  assign(paste0('p', i), p)

}

# # for ref
# din <- which(mods$resvar == 'din')
# nh <- which(mods$resvar == 'nh')
# no23 <- which(mods$resvar == 'no23')

ylab1 <- gsub('ln-', '', as.character(attr(mods$data[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$data[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$data[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 3, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.025, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, p21, p24, p27, ncol = 1)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### NH4

```r
grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 3, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.025, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, p19, p22, p25, ncol = 1)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### NO23

```r
grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 3, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.025, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, p20, p23, p26, ncol = 1)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Nutrients by season by year {.tabset}

The plots below show seasonal changes in flow-normalized results over time using a common-axis that shows only day of the year.  They are similar to the above plots except changes in response to flow are not explicitly included. As above, the lines are specific to different years in the time series.  Excluding specific information about flow response, most of the points for the above plots apply to those below.  The plots can be used as follows:

* Changes by season - from left to right, how do nutrients change throughout the year? 
* Changes by year - variation by color, do average nutrient concentrations change throughout the time series?
* Changes by year and season - is there an interaction between the two?  That is, does the seasonal change from left the right vary given the year?  
* Differences by location - how do the plots change with the site given any of the above information?  
* Differences by nutrient species - how do the plots change with the nutrient species given any of the above information?  

#### DIN

```r
data(mods)

# y axis limits for each plot
lims <- data.frame(
  var = c('din', 'nh', 'no23'),
  upy = c(2.8, 0.9, 2.1)
)

# y axis limits
lims <- data.frame(Delta = c(2.8, 0.9, 2.1), Middle = c(1, 0.175, 0.95), Suisun = c(0.75, 0.175, 0.6))
row.names(lims) <- c('din', 'nh', 'no23')

for(i in 1:nrow(mods)){

  toplo <- mods$data[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  # axis limits by resp var and location var
  resv <- as.character(mods[i, 'resvar'])
  locv <- as.character(mods[i, 'Location'])
  limy <- lims[resv, locv]
    
  p <- seasyrplot(toplo, predicted = F, logspace = F) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(
      axis.title = element_blank(), 
      legend.position = 'top',
      axis.line.x = element_line(size = 0.5),
      axis.line.y = element_line(size = 0.5) 
      ) +
    scale_y_continuous(limits = c(0, limy))

  # get legend
  if(i == 1) pleg <- g_legend(p)
  p <- p + theme(legend.position = 'none')

  assign(paste0('p', i), p)

}

ylab1 <- gsub('ln-', '', as.character(attr(mods$data[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$data[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$data[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, p21, p24, p27, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#### NH4

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, p19, p22, p25, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

#### NO23

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, p20, p23, p26, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### Trend maps {.tabset}

Trends are reported as percent changes of annual averages from the beginning to the end of each period. For monthly trends, percent changes are based on an average of the first three and last three annual averages to reduce the effects of odd years at the beginning and end of each period. For example, percent changes for January throughout a time series from 1980 to 2000 would be the change of the average from January in 1980-1982 to the average from January in 1998-2000. Annual trends, e.g., percent changes from 1980-1986, 1987-1993, etc. do not average by the first and last three years in each grouping because the values are already based on annual averages.  All trends are based on back-transformed, flow-normalized results.

#### DIN


```r
trnd_map(res = 'din', mods = mods)
```

![](README_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### NH4


```r
trnd_map(res = 'nh', mods = mods)
```

![](README_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### NO23


```r
trnd_map(res = 'no23', mods = mods)
```

![](README_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

### Comparison of trends and effects of flow {.tabset}

Seasonal Kendall tests were used to characterize trends using results from the WRTDS models. Trends were compared on the predicted and flow-normalized values to identify potential effects of flow.  Trends were evaluated for different annual groupings and seasonal groupings within each annual group.


```r
data(trnds_seasyr)

# combine all
trnds_seasyr <- rbind(trnds_nrm, trnds_fit, trnds_obs) %>% 
  select(Site_Code, resvar, cat, perchg, ann, trndvar) %>% 
  spread(trndvar, perchg) %>% 
  mutate(Site_Code = factor(Site_Code, levels = c('D7', 'D6', 'D4', 'D28', 'D26', 'D19', 'P8', 'C10', 'C3')))

# response labels 
reslabs <- list(
  shrt = c('din', 'nh', 'no23'),
  expr = c(
    'DIN', 
    expression(paste(NH [4] ^ '+')),
    expression(paste(NO [2] ^ '-', ' / ', NO [3] ^ '2-'))
  )
)

# year only
toplo1 <- filter(trnds_seasyr, cat %in% c('1976-1995', '1996-2014')) %>% 
  gather('var', 'val', fits:norm) %>% 
  mutate(
    resvar = factor(resvar, levels = reslabs$shrt, labels = reslabs$expr), 
    var = factor(var, levels = c('fits', 'norm'), labels = c('Predicted', 'Flow-normalized'))
  )

# seasonal, first years
toplo2 <- filter(trnds_seasyr, !cat %in% c('1976-1995', '1996-2014')& ann == 'bef') %>% 
  gather('var', 'val', fits:norm) %>% 
  mutate(
    resvar = factor(resvar, levels = reslabs$shrt, labels = reslabs$expr), 
    var = factor(var, levels = c('fits', 'norm'), labels = c('Predicted', 'Flow-normalized'))
  )

# seasonal, first years
toplo3 <- filter(trnds_seasyr, !cat %in% c('1976-1995', '1996-2014')& ann == 'aft') %>% 
  gather('var', 'val', fits:norm) %>% 
  mutate(
    resvar = factor(resvar, levels = reslabs$shrt, labels = reslabs$expr), 
    var = factor(var, levels = c('fits', 'norm'), labels = c('Predicted', 'Flow-normalized'))
  )

mythm <- theme_minimal() +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(), 
    axis.title.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(.1, "cm")
  )  
    
p1 <- ggplot(toplo1, aes(x = val, y = Site_Code)) + 
  geom_point(aes(pch = var), size = 3, alpha = 0.8) + 
  facet_grid(cat~resvar, labeller = label_parsed
    ) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  mythm + 
  xlab(expression('% chg yr' ^-1)) +
  scale_shape_manual(values = c(16, 1)) + 
  ggtitle('Annual groups')

p2 <- ggplot(toplo2, aes(x = val, y = Site_Code)) + 
  geom_point(aes(pch = var), size = 3, alpha = 0.8) + 
  facet_grid(cat~resvar, labeller = label_parsed
    ) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  mythm + 
  xlab(expression('% chg yr' ^-1)) +
  scale_shape_manual(values = c(16, 1)) + 
  ggtitle('Seasonal groups, 1976-1995')

p3 <- ggplot(toplo3, aes(x = val, y = Site_Code)) + 
  geom_point(aes(pch = var), size = 3, alpha = 0.8) + 
  facet_grid(cat~resvar, labeller = label_parsed
    ) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  mythm + 
  xlab(expression('% chg yr' ^-1)) +
  scale_shape_manual(values = c(16, 1)) + 
  ggtitle('Seasonal groups, 1996-2014')
```

#### Annual groups


```r
p1
```

![](README_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### Seasonal groups, 1976-1995


```r
p2
```

![](README_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#### Seasonal groups, 1996-2014


```r
p3
```

![](README_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

