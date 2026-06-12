# plot() methods for the ensemble-analysis result classes defined in ens.
# These let `plot(result)` dispatch to the dedicated plotting functions,
# completing the two-layer convention: ens attaches the class and a print
# method; lipdViz provides the plot method. The underlying plot* functions
# remain exported for direct use with explicit arguments.

#' Plot an ensemble correlation result
#' @param x a corEns result (from ens::corEns)
#' @param ... passed to plotCorEns
#' @return a ggplot object
#' @export
plot.corEns <- function(x, ...) {
  plotCorEns(x, ...)
}

#' Plot an ensemble regression result
#' @param x a regressEns result (from ens::regressEns)
#' @param ... passed to plotRegressEns
#' @return a list of ggplot objects
#' @export
plot.regressEns <- function(x, ...) {
  plotRegressEns(x, ...)
}

#' Plot an ensemble spectra result
#' @param x a spectraEns result (from ens::computeSpectraEns)
#' @param ... passed to plotSpectraEns
#' @return a ggplot object
#' @export
plot.spectraEns <- function(x, ...) {
  plotSpectraEns(x, ...)
}

#' Plot an ensemble PCA result
#' @param x a pcaEns result (from ens::pcaEns)
#' @param TS the timeseries object used in the pcaEns analysis (required by plotPcaEns)
#' @param ... passed to plotPcaEns
#' @return a gridExtra ggplot object
#' @export
plot.pcaEns <- function(x, TS, ...) {
  plotPcaEns(x, TS = TS, ...)
}
