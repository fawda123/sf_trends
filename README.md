# README
Marcus W. Beck, beck.marcus@epa.gov  

### Files

**_data/_** Supporting RData files, usually from data in ignore folder 

* `delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results, processed in `R/dat_proc.R`

* `dwr_wq.RData` time series data of stations in the SF delta from California DWR-EMP (Department of Water Resources, Environmental Monitoring Program) , processed by E. Novick, all stations, analytes from 1975 to present.  Most analytes are measured as concentration, see original spreadsheet for values.  Unavailable in GitHub repo.

* `flocor.RData` data.frame of each wq station in `delt_dat.RData` compared with each flow time series in `flow_dat.RData`, lags and correlations for each are shown starting with zero lag back to the previous twelve months, only negative lags are evaluated, i.e, how far back (in months) are the flow time series correlated with nuts, created in `R/dat_proc.R`

* `flow_dat.RData` time series of daily flow estimates for the delta, input stations from Novick et al (Fig 2) were used, created in `R/dat_proc.R` 

* `mods.RData` weighted regression models for 11 delta stations, created in `R/dat_proc.R` 

* `mods_opt.RData` results from `winsrch_optim` that identifies optimal half-window widths for weighted regression models,  created in `R/dat_proc.R`

**_R/_** Supporting R scripts

**_text/_** Summary text of analyses

### Matching flow data with nutrient time series



Monthly nutrient samples at each of the active stations in the Delta were compared with daily flow data to identify the most relevant flow estimates for modelling.  Time series were compared using cross-correlation analysis to identify the minimum negative correlation between flow and nutrient concentration and the corresponding lag at which the minimum correlation was observed.  The time series comparisons were based on observed nitrite/nitrate and seasonal means of flow (monthly means of daily records by year).  Flow estimates in Novick et al. were used: Sacramento River plus Yolo bypass (`sacyolo = sac + yolo`), eastern tributaries (`east = csmr + moke + misc`), and San Joaquin River (`sjr`). 

![Cross-correlation analysis of flow records with nitrite/nitrate time series active Delta stations](README_files/figure-html/unnamed-chunk-2-1.png)


|site |flo     |lag |        acf|
|:----|:-------|:---|----------:|
|D6   |sacyolo |-3  | -0.4314903|
|D19  |east    |-5  | -0.4489456|
|D4   |east    |-3  | -0.4992088|
|D8   |east    |-4  | -0.4903015|
|C10  |east    |-1  | -0.5927893|
|MD10 |east    |-4  | -0.3438148|
|C3   |east    |-4  | -0.3121967|
|D28A |east    |-5  | -0.3819177|
|D26  |sjr     |-5  | -0.4379171|
|D7   |sjr     |-4  | -0.4920025|
|P8   |sjr     |-1  | -0.4572284|

The flow record that had the minimum correlation with each nutrient station was then smoothed using a left-centered moving window average that had a window equal in length to the corresponding lag in the above table.  The smoothed flow records were then matched with the monthly nutrient records for the corresponding station.  

![Nitrogen time series versus matched flow records before and after averaging by the maximum lag.](README_files/figure-html/unnamed-chunk-4-1.png)

Using the combined flow and nutrient time series, the optimal window widths for each station were identified using functions in the [WRTDStidal](https://github.com/fawda123/WRTDStidal) package.  The `winsrch_optim` function uses model cross-validation to iteratively evaluate multiple half-window widths.  The optimal parameter set is identified based on a minimization of error on a test dataset for multiple subsets of the data.  This method attempts to minimize the tradeoff between over- and under-fitting a model with window widths that are too narrow or too wide, respectively.  The following shows the optimal half-window width combinations identified for each location.  


|site | days| years| flow|
|:----|----:|-----:|----:|
|D19  | 0.49|  9.96| 0.50|
|D26  | 0.42|  8.74| 0.50|
|D4   | 0.44|  8.80| 0.25|
|D6   | 0.50| 10.01| 0.50|
|D7   | 0.50| 10.00| 0.50|
|D8   | 0.37| 10.04| 0.10|
|C10  | 0.34|  7.13| 0.43|
|MD10 | 0.12| 10.00| 0.51|
|C3   | 0.50| 10.00| 0.50|
|D28A | 0.47|  4.84| 0.72|
|P8   | 0.26|  9.57| 0.49|

The model predictions and flow-normalized results as annual means are shown below for each of the sites.  The optimal half-window width combinations shown above were used to create each model.  Each color represents a different conditional quantile model fit to the observed time series, i.e., the tenth, fiftieth, and ninetieth percentile distributions.  Note changes in scale, different trends between quantiles, and differences between prediction and flow-normalized results.

![](README_files/figure-html/unnamed-chunk-6-1.png)

Seasonal variation across all years is viewed using a simple loess (locally estimated) polynomial fit through the model results for the observed data. The points show the observed data at each station, whereas the fit is smoothed through the model predictions (not shown on the plot).

![](README_files/figure-html/unnamed-chunk-7-1.png)

### To do 

* get detection limits

