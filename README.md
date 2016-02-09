# README
Marcus W. Beck, beck.marcus@epa.gov  

### Files

**_data/_** Supporting RData files, usually from data in ignore folder 

* `delt_dat.RData` Processed wq time series data `dwr_wq.RData`, includes all nitrogen analytes and current/active stations in the delta, processed in `dat_proc.R`

* `dwr_wq.RData` time series data of stations in the SF delta from California DWR-EMP (Department of Water Resources, Environmental Monitoring Program) , processed by E. Novick, all stations, analytes from 1975 to present.  Most analytes are measured as concentration, see original spreadsheet for values. 

* `flow_dat.RData` time series of daily flow estimates for the delta

* `res.RData` sample results for the delta

**_R/_** Supporting R scripts

**_text/_** Summary text of analyses

### To do

* Check different moving window averages on flow, which fit best to data
* Check which flow stations (or salinity) are best correlated to response vars

