# lipdViz 0.1.2

* New `plotTs()` plots a single timeseries (one element of a TS list, or one row of a timeseries tibble) against its time axis, with axis labels and a title from its metadata. It uses `year` if the record has it and `age` otherwise, plots ages BP on a reversed axis, and `secondary.axis = TRUE` adds the other time scale (yr BP or year CE, relative to 1950). Ensembles and multiple timeseries get an error pointing to `plotTimeseriesEnsRibbons()` and `plotTimeseriesStack()`.

# lipdViz 0.1.1

* Fixed `plotTimeseriesStack()` failing under ggplot2 >= 4.0 with `Can't convert x <double[,1]> to <double>`. Using `invert.var` created a single-column matrix that ggplot2 4 no longer accepts. [See geoChronR issue 91](https://github.com/nickmckay/GeoChronR/issues/91).
* `plotTimeseriesStack()` now accepts TS lists and wide `lipd_ts_tibble` objects (one row per timeseries), converting them to long format with `tidyTs()` internally, rather than failing in `scale()`. Rows with non-finite time values are also dropped.
* Requires lipdR >= 0.7.0.
