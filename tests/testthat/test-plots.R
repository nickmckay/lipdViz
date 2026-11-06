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
  expect_error(plotTs(ts), "neither year nor age")
  ts <- makeTs(); ts$paleoData_values <- matrix(0, 201, 3)
  expect_error(plotTs(ts), "ensembles")
})
