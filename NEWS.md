# lipdViz 0.1.2

* `plotChronEns()` no longer shows a spurious reversal at the ends of an age-depth model. Its ribbons and median are now quantiles of age at each depth of the ensemble table, so they are monotonic whenever the ensemble members are. They used to be quantiles of depth at fixed ages, which is biased near the oldest and youngest ages, where only some ensemble members reach that age; the median could hook back to shallower depths at the base of the core. The `x.bin`, `y.bin` and `n.bins` arguments are now ignored.

# lipdViz 0.1.1

* Fixed `plotTimeseriesStack()` failing under ggplot2 >= 4.0 with `Can't convert x <double[,1]> to <double>`. Using `invert.var` created a single-column matrix that ggplot2 4 no longer accepts. [See geoChronR issue 91](https://github.com/nickmckay/GeoChronR/issues/91).
* `plotTimeseriesStack()` now accepts TS lists and wide `lipd_ts_tibble` objects (one row per timeseries), converting them to long format with `tidyTs()` internally, rather than failing in `scale()`. Rows with non-finite time values are also dropped.
* Requires lipdR >= 0.7.0.
