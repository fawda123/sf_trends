# README
Marcus W. Beck, beck.marcus@epa.gov  

### Files

**_data/_** Supporting RData files, usually from data in ignore folder, unless otherwise noted all files were created in `R/dat_proc.R`

* `delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, also includes matched and smoothed flow records from `flocor.RData` results

* `dwr_wq.RData` time series data of stations in the SF delta from California DWR-EMP (Department of Water Resources, Environmental Monitoring Program) , processed by E. Novick, all stations, analytes from 1975 to present.  Most analytes are measured as concentration, see original spreadsheet for values.  Unavailable in GitHub repo.

* `flocor.RData` results of ccf analysis of selected delta and suisun stations comparing nitrogen and flow

* `flow_dat.RData` time series of daily flow estimates for the delta, input stations from Novick et al (Fig 2) were used

* `nutcor.RData` results of ccf analysis of selected delta and suisun stations comparing nitrogen species

**_R/_** Supporting R scripts

**_text/_** Summary text of analyses

### Comparing time series of nutrients and flow for selected stations



Monthly nutrient samples at selected stations were compared with flow estimates to characterize variation in time series correlations. Time series were compared using cross-correlation analysis with lags +/- 12 months.  Selected stations from the delta were C10, C, and P8 and selected stations from Suisun were D4, D6, and D7.  Flow estimates from Novick et al. were the Sacramento River plus Yolo bypass (`sacyolo = sac + yolo`),  San Joaquin River (`sjr`).  Nutrient data were also compared with salinity observations at each station.  Nitrogen species evaluated included dissolved inorganic nitrogen (`din`), ammonium (`nh`), and nitrite/nitrate (`no23`). 

![](README_files/figure-html/unnamed-chunk-2-1.png)
  
The above analysis was repeated to compare temporal variation of nitrogen species.  

![](README_files/figure-html/unnamed-chunk-3-1.png)

<img src="README_files/figure-html/unnamed-chunk-4-1.png" title="" alt="" width="400px" /><img src="README_files/figure-html/unnamed-chunk-4-2.png" title="" alt="" width="400px" /><img src="README_files/figure-html/unnamed-chunk-4-3.png" title="" alt="" width="400px" /><img src="README_files/figure-html/unnamed-chunk-4-4.png" title="" alt="" width="400px" /><img src="README_files/figure-html/unnamed-chunk-4-5.png" title="" alt="" width="400px" /><img src="README_files/figure-html/unnamed-chunk-4-6.png" title="" alt="" width="400px" />

### Matching stations to flow or salinity records

Stations were 

### To do 

* get detection limits

