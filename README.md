# lipdViz

Visualization for [LiPD](https://lipd.net) data and time-uncertain ensemble analyses.

`lipdViz` provides the plotting and mapping toolkit that was previously part of
[geoChronR](https://github.com/nickmckay/GeoChronR):

- **Age model ensembles**: `plotChronEns()`, `plotChron()`, `plotModelDistributions()`, `plotChronEnsDiff()`
- **Timeseries with uncertainty**: `plotTimeseriesEnsRibbons()`, `plotTimeseriesEnsLines()`, `plotTimeseriesStack()`
- **Analysis results**: `plotCorEns()`, `plotRegressEns()`, `plotSpectraEns()`, `plotPcaEns()`, `plotScreeEns()`, `plotHistEns()`
- **Maps**: `mapLipd()`, `mapTs()`, `baseMap()`, `plotTimeAvailabilityTs()`
- **Dataset summaries**: `plotSummary()`, `plotSummaryTs()`

## Installation

```r
remotes::install_github("nickmckay/lipdViz")
```

## Related packages

- [ens](https://github.com/nickmckay/ens) — ensemble methods and calculations (imported by this package)
- [geoChronR](https://github.com/nickmckay/geoChronR-chronOnly) — age modeling (Bacon, Bchron, OxCal, BAM)
- [lipdR](https://github.com/nickmckay/lipdR) — reading and writing LiPD files
