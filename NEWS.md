# lipdViz 0.2.1

* `plotChronEns()` no longer shows a spurious reversal at the ends of an age-depth model. Its ribbons and median are now quantiles of age at each depth of the ensemble table, so they are monotonic whenever the ensemble members are. They used to be quantiles of depth at fixed ages, which is biased near the oldest and youngest ages, where only some ensemble members reach that age; the median could hook back to shallower depths at the base of the core. The `x.bin`, `y.bin` and `n.bins` arguments are now ignored.

# lipdViz 0.2.0

* `plotTimeAvailabilityTs()` and `plotSummaryTs()` now use each record's `time` by default (`age.var = "time"`), converted to a common scale with the new `time.units` argument (default "yr BP"), using the time metadata from `lipdR::extractTs()`. Records in year CE and yr BP are counted together; with `time.units = "CE"` the axis runs forward. An explicit `age.var` (e.g. "age") works as before. Requires lipdR >= 0.7.3.
* `plotTs()` uses `time` and its metadata by default, so a record whose primary time column is year CE is plotted in year CE. The new `time.units` argument converts the axis (e.g. "ka"), and the secondary axis is computed with `lipdR::convertTime()`, so it is right for ka and b2k axes too.
* `plotTimeAvailabilityTs()`, `plotSummaryTs()` and `plotPcaEns()` accept timeseries tibbles (`lipdR::ts2tibble()`). They passed tibbles to `lipdR::untidyTs()`, which expects the long `tidyTs()` format, so tibble input failed. `mapTs()` also accepts plain data frames.

# lipdViz 0.1.2

* New `plotTs()` plots a single timeseries (one element of a TS list, or one row of a timeseries tibble) against its time axis, with axis labels and a title from its metadata. It uses `year` if the record has it and `age` otherwise, plots ages BP on a reversed axis, and `secondary.axis = TRUE` adds the other time scale (yr BP or year CE, relative to 1950). Ensembles and multiple timeseries get an error pointing to `plotTimeseriesEnsRibbons()` and `plotTimeseriesStack()`.

# lipdViz 0.1.1

* Fixed `plotTimeseriesStack()` failing under ggplot2 >= 4.0 with `Can't convert x <double[,1]> to <double>`. Using `invert.var` created a single-column matrix that ggplot2 4 no longer accepts. [See geoChronR issue 91](https://github.com/nickmckay/GeoChronR/issues/91).
* `plotTimeseriesStack()` now accepts TS lists and wide `lipd_ts_tibble` objects (one row per timeseries), converting them to long format with `tidyTs()` internally, rather than failing in `scale()`. Rows with non-finite time values are also dropped.
* Requires lipdR >= 0.7.0.
