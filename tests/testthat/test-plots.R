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
