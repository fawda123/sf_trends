# README
Marcus W. Beck, beck.marcus@epa.gov  

Content available at [https://github.com/fawda123/sf_trends](https://github.com/fawda123/sf_trends)

### Files

**_data/_** Supporting RData files, usually from data in ignore folder, unless otherwise noted all files were created in `R/dat_proc.R`

* `bests.RData` subset of `flocor.RData` for min or max cor of each nut parm with flow or sal time series, respectively.  Each site is matched with the flow or salinity record ided in figure 10 of Novick et al

* `delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results

* `dwr_wq.RData` time series data of stations in the SF delta from California DWR-EMP (Department of Water Resources, Environmental Monitoring Program) , processed by E. Novick, all stations, analytes from 1975 to present.  Most analytes are measured as concentration, see original spreadsheet for values.  Unavailable in GitHub repo.

* `flocor.RData` results of ccf analysis of selected delta and suisun stations comparing nitrogen and flow

* `flow_dat.RData` time series of daily flow estimates for the delta, input stations from Novick et al (Fig 2) were used

* `nutcor.RData` results of ccf analysis of selected delta and suisun stations comparing nitrogen species

* `mods.RData` dataset for wrtds, including model results. This is a nested data frame with identifiers.  All response, flow values are ln + 1 transformed, flow (or salinity) records for each nutrient variable and station are combined based on the monthly lag ided from `bests.RData`

* `mods_nolag.RData` dataset for wrtds, including model results.  This is the same file as `mods.RData` except the matched flow variables are not lagged.

**_R/_** Supporting R scripts

**_text/_** Summary text of analyses

### Comparing time series of nutrients and flow for selected stations {.tabset}


```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(WRTDStidal)
library(gridExtra)
library(lubridate)
library(purrr)
library(GGally)
source('R/funcs.R')
```

Monthly nutrient samples at selected stations were compared with flow estimates to characterize variation in time series correlations. Time series were compared using cross-correlation analysis with lags +/- 12 months.  Selected stations from the delta were C10, C3, and P8 and selected stations from Suisun were D4, D6, and D7.  Flow estimates from Novick et al. were the Sacramento River plus Yolo bypass (`sacyolo = sac + yolo`),  San Joaquin River (`sjr`).  Nutrient data were also compared with salinity observations at each station.  Nitrogen species evaluated included dissolved inorganic nitrogen (`din`), ammonium (`nh`), and nitrite/nitrate (`no23`). 

#### Nutrients to flow variables 


```r
data(flocor)

ggplot(flocor, aes(x = lag, y = acf, colour = flovar, group = flovar)) +
  facet_grid(Location + Site_Code ~ resvar) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous('lag (months)') +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())
```

![](README_files/figure-html/unnamed-chunk-2-1.png)
  
#### Nutrients to nutrients
The above analysis was repeated to compare temporal variation of nitrogen species.  


```r
data(nutcor)

ggplot(nutcor, aes(x = lag, y = acf, colour = var2, group = var2)) +
  facet_grid(Location + Site_Code ~ var1) +
  geom_line() + 
  geom_point() + 
  scale_x_continuous('lag (months)') +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())
```

![](README_files/figure-html/unnamed-chunk-3-1.png)

### Matching stations to flow or salinity records

This table shows the preceding lag in months at which the correlation of the flow variable was highest (as an absolute magnitude) for the corresponding site and nutrient variable.  Flow variables matched with each nutrient station were those with the nearest geographic location, e.g., C10 is near the inflow of San Joaquin into the delta, D4 is closer to the ocean and was matched wtih salinity, etc.  Nutrient data were matched with the flow/salinity time series at the corresponding lag to develop weighted regression models.  For example, a maximum lag of -4 means that the flow record four months prior to the nutrient record was matched on the observed date of the nutrient sample. 

```r
data(bests)
  
knitr::kable(bests, format = 'markdown', caption = 'Minimum correlations and lags of each nutrient station that corresponded to the flow estimates')
```



|Site_Code |Location |resvar |flovar      | lag|        acf|
|:---------|:--------|:------|:-----------|---:|----------:|
|C10       |Delta    |din    |San Joaquin |   0| -0.6541804|
|C10       |Delta    |nh     |San Joaquin |  -3| -0.1250742|
|C10       |Delta    |no23   |San Joaquin |   0| -0.6769895|
|C3        |Delta    |din    |Sacramento  |   0| -0.5375051|
|C3        |Delta    |nh     |Sacramento  |   0| -0.7299349|
|C3        |Delta    |no23   |Sacramento  |  -4| -0.2635292|
|P8        |Delta    |din    |San Joaquin |   0| -0.4399586|
|P8        |Delta    |nh     |San Joaquin |  -3| -0.1641490|
|P8        |Delta    |no23   |San Joaquin |   0| -0.4794830|
|D4        |Suisun   |din    |Salinity    |  -3|  0.5319224|
|D4        |Suisun   |nh     |Salinity    |  -2|  0.3478721|
|D4        |Suisun   |no23   |Salinity    |  -3|  0.5023232|
|D6        |Suisun   |din    |Salinity    |  -2|  0.4032670|
|D6        |Suisun   |nh     |Salinity    |  -1|  0.3501523|
|D6        |Suisun   |no23   |Salinity    |  -3|  0.3417281|
|D7        |Suisun   |din    |Salinity    |  -3|  0.4688692|
|D7        |Suisun   |nh     |Salinity    |  -2|  0.4210653|
|D7        |Suisun   |no23   |Salinity    |  -4|  0.3972676|

### Annually-averaged results {.tabset}

The plots below show annually-averaged results of weighted regression for each station using the nutrient records and matched flow/salinity data.  The three lines in each plot represent model results for the conditional distributions of the 10th, 50th, and 90th percentiles of the nutrient record.  Points represent model predictions and lines are flow-normalized predictions that show trends independent of flow variation.The following describes points of interest that can be idenfied from the plots:

* General trends - flow-normalized trends over time are the most descriptive of changes, are nutrients decreasing, inreasing, or constant?
* Differences in the percentiles - variation in the 10th or 90th percentile distributions that differ from the median response suggest changes in frequency occurrence of low or high nutrient events, respectively.  In other words, the median response does not tell the whole picture about change in nutrient concentrations over time.  
* Differences in flow-normalized predictions and observed predictions - large differences between the two represent either a large effect of flow or an inappropriate flow variable
* Differences in the magnitude of the modelled response between locations - differences can show the relative proportions of nitrogen species at each site.  This is why the y-axis limits are constant for each response measure.
* Differences by nutrient species - how do the plots change with the nutrient species given any of the above information?  

#### DIN

```r
data(mods)
lims <- data.frame(
  var = c('din', 'nh', 'no23'),
  upy = c(3, 0.85, 2.6)
)

for(i in 1:nrow(mods)){

  toplo <- mods$mod[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  limy <- as.character(mods[i, 'resvar'])
  limy <- lims[lims$var == limy, 'upy']
    
  p <- prdnrmplot(toplo, logspace = F) +
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

ylab1 <- gsub('ln-', '', as.character(attr(mods$mod[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$mod[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$mod[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)

#### NH4

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-6-1.png)

#### NO23 

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-7-1.png)

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

# y axis limits for each plot
lims <- data.frame(
  var = c('din', 'nh', 'no23'),
  upy = c(3, 1, 2.8)
)

for(i in 1:nrow(mods)){

  toplo <- mods$mod[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  limy <- as.character(mods[i, 'resvar'])
  limy <- lims[lims$var == limy, 'upy']
    
  p <- dynaplot(toplo, month = c(1, 4, 7, 10), ncol = 1, logspace = F) +
    ggtitle(lab) + 
    theme_minimal() +
    theme(
      axis.title = element_blank(), 
      legend.position = 'top', 
      axis.line.x = element_line(size = 0.5),
      axis.line.y = element_line(size = 0.5) 
      ) +
    # scale_x_continuous(limits = c(0, 1)) +
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

ylab1 <- gsub('ln-', '', as.character(attr(mods$mod[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$mod[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$mod[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 1, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.05, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, ncol = 6)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-8-1.png)

#### NH4

```r
grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 1, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.05, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, ncol = 6)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-9-1.png)

#### NO23

```r
grid.arrange(
  pleg, nrow = 3, heights = c(0.1, 1, 0.1), 
  arrangeGrob(
  ncol = 2, widths = c(0.05, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, ncol = 6)
  ), 
  grid::textGrob('Flow or Salinity (standardized)')
)
```

![](README_files/figure-html/unnamed-chunk-10-1.png)

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
  upy = c(2.8, 0.8, 2.1)
)

for(i in 1:nrow(mods)){

  toplo <- mods$mod[[i]]
  lab <- paste(mods[i, 'Location'], mods[i, 'Site_Code'], sep = ', ')
  
  limy <- as.character(mods[i, 'resvar'])
  limy <- lims[lims$var == limy, 'upy']
    
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

ylab1 <- gsub('ln-', '', as.character(attr(mods$mod[[1]], 'reslab')))
ylab1 <- parse(text = ylab1)
ylab2 <- gsub('ln-', '', as.character(attr(mods$mod[[2]], 'reslab')))
ylab2 <- parse(text = ylab2)
ylab3 <- gsub('ln-', '', as.character(attr(mods$mod[[3]], 'reslab')))
ylab3 <- parse(text = ylab3)

grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab1, rot = 90), 
    arrangeGrob(p1, p4, p7, p10, p13, p16, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-11-1.png)

#### NH4

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab2, rot = 90), 
    arrangeGrob(p2, p5, p8, p11, p14, p17, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-12-1.png)

#### NO23

```r
grid.arrange(
  pleg, nrow = 2, heights = c(0.1, 1), 
  arrangeGrob(
  ncol = 2, widths = c(0.1, 1), 
    grid::textGrob(ylab3, rot = 90), 
    arrangeGrob(p3, p6, p9, p12, p15, p18, ncol = 3)
  )
)
```

![](README_files/figure-html/unnamed-chunk-13-1.png)

### Effect of using lagged flow or salinity variables {.tabset}

The effect of using lagged or no lagged flow or salinity variables on model performance was evaluated with residual errors.  Models were fit using the 'optimal' lags from above and using no lags.  The plot below shows root mean squared error for the different models fit, using the conditional median as an example. The identified lag for each station, variable combination is below each bar. Using lagged variables causes an increase in error and reduction in performance. 

#### 0.1

```r
# load the data
data(mods)
data(mods_nolag)
data(bests)

## get model performance from the data objects

# models w/ lagged flo variables
wlag <- map(mods$mod, wrtdsperf) %>% 
  do.call('rbind', .) %>% 
  mutate(
    lab = gsub('\\.[0-9]$', '', row.names(.)), 
    typ = 'With lag'
    ) %>% 
  separate(lab, c('stat', 'var'), sep = '_')

# models w/o no lagged variables
nlag <- map(mods_nolag$mod, wrtdsperf) %>% 
  do.call('rbind', .) %>% 
  mutate(
    lab = gsub('\\.[0-9]$', '', row.names(.)), 
    typ = 'No lag'
    ) %>% 
  separate(lab, c('stat', 'var'), sep = '_')

# combine for plotting, extract results for median mod
perfs <- rbind(wlag, nlag)

# txt to add to the plot showing lag
txt <- rename(bests, 
  var = resvar, 
  stat = Site_Code
)

# color vector
cols <- RColorBrewer::brewer.pal(9, 'Set1')[c(2, 3)]

# the plot
ggplot(perfs[perfs$tau == 0.1,], aes(x = stat, y = rmse)) +
  geom_bar(aes(fill = typ), stat = 'identity', position = 'dodge', alpha = 0.75) + 
  geom_text(data = txt, aes(y = -0.007, label = lag)) +
  facet_grid(~var) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    legend.position = 'top',
    legend.title = element_blank(), 
    axis.title.x= element_blank()
    ) + 
  scale_fill_manual(values = cols)
```

![](README_files/figure-html/unnamed-chunk-14-1.png)

#### 0.5

```r
# the plot
ggplot(perfs[perfs$tau == 0.5,], aes(x = stat, y = rmse)) +
  geom_bar(aes(fill = typ), stat = 'identity', position = 'dodge', alpha = 0.75) + 
  geom_text(data = txt, aes(y = -0.007, label = lag)) +
  facet_grid(~var) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    legend.position = 'top',
    legend.title = element_blank(), 
    axis.title.x= element_blank()
    ) + 
  scale_fill_manual(values = cols)
```

![](README_files/figure-html/unnamed-chunk-15-1.png)

#### 0.9

```r
# the plot
ggplot(perfs[perfs$tau == 0.9,], aes(x = stat, y = rmse)) +
  geom_bar(aes(fill = typ), stat = 'identity', position = 'dodge', alpha = 0.75) + 
  geom_text(data = txt, aes(y = -0.007, label = lag)) +
  facet_grid(~var) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(size = 0.5),
    axis.line.y = element_line(size = 0.5),
    legend.position = 'top',
    legend.title = element_blank(), 
    axis.title.x= element_blank()
    ) + 
  scale_fill_manual(values = cols)
```

![](README_files/figure-html/unnamed-chunk-16-1.png)

### To do 

* get detection limits

