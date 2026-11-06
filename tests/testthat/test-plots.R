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
