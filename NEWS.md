# lipdViz 0.1.1

* Fixed `plotTimeseriesStack()` failing under ggplot2 >= 4.0 with `Can't convert x <double[,1]> to <double>`. Using `invert.var` created a single-column matrix that ggplot2 4 no longer accepts. [See geoChronR issue 91](https://github.com/nickmckay/GeoChronR/issues/91).
* `plotTimeseriesStack()` now accepts TS lists and wide `lipd_ts_tibble` objects (one row per timeseries), converting them to long format with `tidyTs()` internally, rather than failing in `scale()`. Rows with non-finite time values are also dropped.
* Requires lipdR >= 0.7.0.
