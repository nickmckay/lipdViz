library(lipdViz)

# synthetic ensemble data shared across tests
makeEns <- function() {
  set.seed(42)
  nobs <- 80
  n.ens <- 30
  list(time = matrix(sort(runif(nobs, 0, 1000)), nobs, n.ens) +
         matrix(rnorm(nobs * n.ens, sd = 5), nobs, n.ens),
       vals = as.matrix(cumsum(rnorm(nobs))))
}

expect_renders <- function(p) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 600, height = 400)
  rendered <- tryCatch({print(p); TRUE}, error = function(e){message(conditionMessage(e)); FALSE})
  grDevices::dev.off()
  expect_true(rendered)
  expect_true(file.exists(f))
}

test_that("plotTimeseriesEnsRibbons renders", {
  d <- makeEns()
  X <- list(values = d$time, variableName = "age", units = "yr BP")
  Y <- list(values = d$vals, variableName = "proxy", units = "unitless")
  expect_renders(plotTimeseriesEnsRibbons(X = X, Y = Y))
})

test_that("plotTimeseriesEnsLines renders", {
  d <- makeEns()
  X <- list(values = d$time, variableName = "age", units = "yr BP")
  Y <- list(values = d$vals, variableName = "proxy", units = "unitless")
  expect_renders(plotTimeseriesEnsLines(X = X, Y = Y, n.ens.plot = 10))
})

test_that("plotHistEns renders with quantile lines", {
  set.seed(1)
  expect_renders(plotHistEns(rnorm(1000)))
})

test_that("plotScreeEns renders", {
  set.seed(4)
  pcaout <- list(variance = matrix(sort(runif(500), decreasing = TRUE), 5, 100),
                 nullVariance = matrix(runif(500, 0, .3), 5, 100))
  expect_renders(plotScreeEns(pcaout))
})

test_that("plotCorEns renders from corEns output", {
  d <- makeEns()
  d2 <- makeEns()
  sink(tempfile())
  co <- ens::corEns(d$time, d$vals, d2$time, d2$vals + rnorm(80),
                    bin.step = 50, isospectral = FALSE, max.ens = 100)
  sink()
  expect_renders(plotCorEns(co, significance.option = "eff-n"))
})

test_that("axisLabel builds labels from variable lists", {
  v <- list(values = 1:5, variableName = "temperature", units = "degC")
  expect_equal(axisLabel(v), "temperature (degC)")
})

test_that("quantile2d returns quantiles on the binned grid", {
  d <- makeEns()
  q <- quantile2d(d$time, matrix(rep(d$vals, 30), nrow(d$vals), 30),
                  n.bins = 50, probs = c(.25, .5, .75))
  expect_equal(ncol(q$quants), 3)
  expect_equal(nrow(q$quants), length(q$x.bin))
  expect_true(all(diff(t(q$quants)) >= 0, na.rm = TRUE)) # quantiles ordered
})

test_that("baseMap returns a ggplot for line maps", {
  m <- baseMap(lon = c(-110, -100), lat = c(35, 45), map.type = "line")
  expect_s3_class(m, "ggplot")
})

# plotChronEns / age-depth ribbons --------------------------------------

makeAgeDepthEns <- function(){
  # 1000 monotonic age-depth models whose basal ages differ widely, the
  # situation that made age-binned quantiles hook back at the ends
  set.seed(7)
  depth <- seq(0, 700, by = 10)
  A <- sapply(1:200, function(i){
    rates <- rexp(length(depth) - 1, rate = 1 / runif(1, 10, 30))
    c(0, cumsum(rates))
  })
  list(depth = depth, A = A)
}

test_that("age-depth ribbons are monotonic in depth", {
  e <- makeAgeDepthEns()
  p <- lipdViz:::plotAgeDepthRibbons(
    ageEnsemble = list(values = e$A, variableName = "ageEnsemble", units = "yr BP"),
    depth = list(values = e$depth, variableName = "depth", units = "cm"))
  b <- ggplot2::ggplot_build(p)
  # the last layer is the median path; every ribbon edge and the median must
  # get older with depth
  for(l in seq_along(b$data)){
    d <- b$data[[l]]
    d <- d[order(d$y), ]
    for(col in intersect(c("x", "xmin", "xmax"), names(d))){
      # x is plotted on a reversed axis, so older ages are more negative
      expect_true(all(diff(d[[col]]) <= 1e-8), info = paste("layer", l, col))
    }
  }
  expect_renders(p)
})

test_that("age-based binning shows the end-of-model artifact that depth quantiles avoid", {
  e <- makeAgeDepthEns()
  q <- quantile2d(e$A, matrix(e$depth, ncol = 1), n.bins = 200, probs = 0.5)
  med <- q$quants[, 1]
  ok <- is.finite(med)
  # depth of the age-binned median decreases somewhere (doubles back)
  expect_true(any(diff(med[ok]) < -1e-8))
})

# time and tibble input for summary plots ------------------------------

test_that("plotTimeAvailabilityTs counts CE and BP records on one scale, from a tibble", {
  yrs <- seq(1005, 1995, by = 10)
  ce <- list(dataSetName = "ce", paleoData_TSid = "a", archiveType = "Wood", paleoData_values = yrs,
             year = yrs, time = yrs, timeUnits = "yr AD", timeDatum = 0,
             timeDirection = "prograde", timeExponent = 0)
  bp <- list(dataSetName = "bp", paleoData_TSid = "b", archiveType = "LakeSediment", paleoData_values = yrs,
             age = 1950 - yrs, ageUnits = "yr BP", time = 1950 - yrs, timeUnits = "yr BP", timeDatum = 1950,
             timeDirection = "retrograde", timeExponent = 0)
  TS <- structure(list(ce, bp), class = c("lipd_ts", "list"))
  p <- plotTimeAvailabilityTs(TS, step = 10)
  expect_equal(p$scales$get_scales("x")$name, "age (yr BP)")
  d <- ggplot2::layer_data(p)
  expect_true(max(d$y) == 2)
  pt <- plotTimeAvailabilityTs(lipdR::ts2tibble(TS), step = 10)
  expect_equal(ggplot2::layer_data(pt)$y, d$y)
  pce <- plotTimeAvailabilityTs(TS, step = 10, time.units = "CE")
  expect_equal(pce$scales$get_scales("x")$name, "year (yr AD)")
  expect_equal(pce$scales$get_scales("x")$trans$name, "identity")
  # another time variable, and the deprecated age.var
  pa <- plotTimeAvailabilityTs(TS[2], time.variable = "age", step = 10)
  expect_equal(pa$scales$get_scales("x")$name, "age (yr BP)")
  expect_warning(pd <- plotTimeAvailabilityTs(TS[2], age.var = "age", step = 10), "deprecated")
  expect_equal(ggplot2::layer_data(pd)$y, ggplot2::layer_data(pa)$y)
  # records on one shared scale stay on it
  expect_equal(plotTimeAvailabilityTs(TS[1], step = 10)$scales$get_scales("x")$name, "year (yr AD)")
})

# plotTs ---------------------------------------------------------------

makeTs <- function(){
  list(dataSetName = "Test.Lake.2026",
       paleoData_variableName = "d18O",
       paleoData_units = "permil",
       year = seq(0, 2000, by = 10),
       yearUnits = "AD",
       age = 1950 - seq(0, 2000, by = 10),
       ageUnits = "BP",
       paleoData_values = sin(seq(0, 2000, by = 10) / 100))
}

test_that("plotTs plots a TS list element, using year by default", {
  p <- plotTs(makeTs())
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "d18O (permil)")
  expect_equal(p$labels$title, "Test.Lake.2026")
  expect_equal(p$scales$get_scales("x")$name, "year (AD)")
  expect_renders(p)
})

test_that("plotTs falls back to age and reverses the axis", {
  ts <- makeTs()
  ts$year <- NULL
  p <- plotTs(ts)
  sx <- p$scales$get_scales("x")
  expect_equal(sx$name, "age (BP)")
  expect_equal(sx$trans$name, "reverse")
  expect_renders(p)
})

test_that("plotTs adds a secondary time axis", {
  p <- plotTs(makeTs(), secondary.axis = TRUE)
  expect_false(inherits(p$scales$get_scales("x")$secondary.axis, "waiver"))
  expect_renders(p)
  ts <- makeTs(); ts$year <- NULL; ts$age <- ts$age / 1000; ts$ageUnits <- "ka"
  expect_renders(plotTs(ts, secondary.axis = TRUE))
})

test_that("plotTs uses lipdR time metadata, and converts with time.units", {
  ts <- makeTs()
  ts$time <- ts$year; ts$timeUnits <- "yr AD"; ts$timeDatum <- 0
  ts$timeDirection <- "prograde"; ts$timeExponent <- 0
  p <- plotTs(ts)
  expect_equal(p$scales$get_scales("x")$name, "year (yr AD)")
  bp <- plotTs(ts, time.units = "ka")
  sx <- bp$scales$get_scales("x")
  expect_equal(sx$name, "age (ka)")
  expect_equal(sx$trans$name, "reverse")
  expect_equal(range(bp$data$time), c(-0.05, 1.95))
  expect_renders(plotTs(ts, time.units = "ka", secondary.axis = TRUE))
  # another time variable, with the record's own label, or converted
  expect_equal(plotTs(ts, time.variable = "age")$scales$get_scales("x")$name, "age (BP)")
  expect_equal(plotTs(ts, time.variable = "age", time.units = "CE")$scales$get_scales("x")$name, "year (yr AD)")
  # the deprecated time.var still works
  expect_warning(pv <- plotTs(ts, time.var = "age"), "deprecated")
  expect_equal(pv$scales$get_scales("x")$name, "age (BP)")
})

test_that("plotTs accepts a one-row tibble", {
  ts <- makeTs()
  tib <- tibble::tibble(dataSetName = ts$dataSetName,
                        paleoData_variableName = ts$paleoData_variableName,
                        paleoData_units = ts$paleoData_units,
                        year = list(ts$year), yearUnits = ts$yearUnits,
                        paleoData_values = list(ts$paleoData_values))
  p <- plotTs(tib)
  expect_equal(p$labels$y, "d18O (permil)")
  expect_renders(p)
  expect_error(plotTs(rbind(tib, tib)), "one timeseries")
})

test_that("plotTs gives helpful errors", {
  expect_error(plotTs(list(makeTs(), makeTs())), "TS list")
  ts <- makeTs(); ts$year <- NULL; ts$age <- NULL
  expect_error(plotTs(ts), "no time, year or age")
  ts <- makeTs(); ts$paleoData_values <- matrix(0, 201, 3)
  expect_error(plotTs(ts), "ensembles")
})

# time scales on the x axis -----------------------------------------------

makeTimeTs <- function(name, values, units){
  ce <- lipdR::isPrograde(units)
  sc <- lipdR::timeScale(units)
  list(dataSetName = name, paleoData_TSid = name, paleoData_variableName = "d18O",
       paleoData_units = "permil", archiveType = "LakeSediment",
       paleoData_values = sin(values / 50),
       time = values, timeUnits = sc$timeUnits, timeDatum = sc$timeDatum,
       timeDirection = sc$timeDirection, timeExponent = sc$timeExponent,
       timeSource = "units", timeVariableName = if(ce) "year" else "age",
       year = if(ce) values, yearUnits = if(ce) "CE",
       age = if(!ce) values, ageUnits = if(!ce) units)
}

test_that("plotTimeseriesStack labels and orients the x axis from the time scale", {
  bp <- structure(list(makeTimeTs("a", seq(0, 1000, 10), "yr BP"),
                       makeTimeTs("b", seq(100, 900, 10), "yr BP")), class = c("lipd_ts", "list"))
  p <- plotTimeseriesStack(bp)
  sx <- p$scales$get_scales("x")
  expect_equal(sx$name, "Age (yr BP)")
  expect_equal(sx$trans$name, "reverse")
  expect_renders(p)

  ce <- structure(list(makeTimeTs("c", seq(1000, 2000, 10), "CE")), class = c("lipd_ts", "list"))
  pc <- plotTimeseriesStack(ce)
  expect_equal(pc$scales$get_scales("x")$name, "Year (yr AD)")
  expect_equal(pc$scales$get_scales("x")$trans$name, "identity")

  # different scales are put on one, with a message
  mixed <- structure(c(bp, ce), class = c("lipd_ts", "list"))
  expect_message(pm <- plotTimeseriesStack(mixed), "different time scales")
  expect_equal(pm$scales$get_scales("x")$name, "Age (yr BP)")
  expect_renders(pm)
  pk <- plotTimeseriesStack(mixed, time.units = "ka")
  expect_equal(pk$scales$get_scales("x")$name, "Age (ka)")
  expect_true(max(pk$data$.plotTime) <= 1.001)

  # a timeseries tibble, a long data frame with time, and the deprecated time.var
  expect_equal(plotTimeseriesStack(lipdR::ts2tibble(bp))$scales$get_scales("x")$name, "Age (yr BP)")
  long <- lipdR::tidyTs(bp, age.var = "time")
  pl <- plotTimeseriesStack(long)
  expect_false(identical(pl$scales$get_scales("x")$name, "Unknown"))
  expect_equal(pl$scales$get_scales("x")$name, "Age (yr BP)")
  expect_warning(pv <- plotTimeseriesStack(bp, time.var = "time"), "deprecated")
  expect_equal(pv$scales$get_scales("x")$name, "Age (yr BP)")
})

test_that("LiPD-object plots take direction from the time scale, and convert", {
  Y <- list(values = sin(1:50), variableName = "proxy", units = "unitless")
  # units that don't say, but a time scale that does
  X <- list(values = seq(1000, 1980, 20), variableName = "year", units = "yr",
            timeUnits = "yr AD", timeDatum = 0, timeDirection = "prograde", timeExponent = 0)
  expect_equal(plotLine(X = X, Y = Y)$scales$get_scales("x")$trans$name, "identity")
  pb <- plotLine(X = X, Y = Y, time.units = "yr BP")
  expect_equal(pb$scales$get_scales("x")$trans$name, "reverse")
  expect_equal(pb$scales$get_scales("x")$name, "year (yr BP)")
  expect_equal(sort(-range(ggplot2::layer_data(pb)$x)), c(-30, 950)) # a reversed scale negates x

  d <- makeEns()
  Xe <- list(values = d$time / 1000, variableName = "ageEnsemble", units = "ka")
  Ye <- list(values = d$vals, variableName = "proxy", units = "unitless")
  pr <- plotTimeseriesEnsRibbons(X = Xe, Y = Ye, time.units = "CE")
  expect_null(pr$scales$get_scales("x")) # not reversed
  expect_true(all(ggplot2::layer_data(pr)$x > 900))
  expect_renders(pr)
  pl <- plotTimeseriesEnsLines(X = Xe, Y = Ye, n.ens.plot = 5)
  expect_equal(pl$scales$get_scales("x")$trans$name, "reverse")

  e <- makeAgeDepthEns()
  pa <- lipdViz:::plotAgeDepthRibbons(
    ageEnsemble = list(values = 1950 - e$A, variableName = "yearEnsemble", units = "yr",
                       timeUnits = "yr AD", timeDatum = 0, timeDirection = "prograde", timeExponent = 0),
    depth = list(values = e$depth, variableName = "depth", units = "cm"))
  expect_null(pa$scales$get_scales("x"))
  expect_error(lipdViz:::convertVarTime(list(values = 1:3, units = "cm", variableName = "depth"), "ka"), "unknown")
})
