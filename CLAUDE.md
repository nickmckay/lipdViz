# CLAUDE.md — lipdViz

`lipdViz` is the **visualization** package of a five-package paleogeoscience family (split out
of the monolithic geoChronR, June 2026). All plotting and mapping for LiPD data and ensemble
analyses lives here.

## Package family (dependency DAG: ens ← lipdViz ← geoChronR; actR & compositeR on top)

| Repo (`~/GitHub/...`) | GitHub | Branch | Role |
|---|---|---|---|
| ens | nickmckay/ens | main | Ensemble methods + UQ engine |
| **lipdViz** (this repo) | nickmckay/lipdViz | main | Plotting + mapping; imports ens |
| geoChronR-chronOnly | nickmckay/geoChronR-chronOnly | main | geoChronR 2.0: age modeling; re-exports ens+lipdViz |
| actR | **LinkedEarth/actR** | refactor | Abrupt-change detection |
| compositeR | nickmckay/compositeR | refactor | Record compositing |

## What lives here

- Plot functions, organized by theme: `R/plot-helpers.R`, `R/plot-spectra.R`,
  `R/plot-timeseries.R`, `R/plot-chron.R`, `R/plot-analysis.R`, plus `R/mapping.R`.
- **`plot.*` methods for ens result classes** (`R/plot-methods.R`): `plot.corEns` /
  `plot.regressEns` / `plot.spectraEns` / `plot.pcaEns` dispatch to the existing
  `plotCorEns` / `plotRegressEns` / `plotSpectraEns` / `plotPcaEns` (which stay exported).
  This is the lipdViz half of the family's two-layer convention (ens attaches the class +
  print method; lipdViz provides plotting).
- Imports `ens` for LiPD data access (`selectData`, `pullTsVariable`,
  `mapAgeEnsembleToPaleoData`) and `fdr`.

## Gotchas

- Targets ggplot2 4.x: use `after_stat()` not `..x..`, `linewidth` not `size`, and never pass
  a stray positional arg to `element_line()` (S7 validation rejects it).
- `@inheritParams ens::selectData` style is required for params defined in ens.
- Plot smoke tests in `tests/testthat/test-plots.R` render to a temp PNG and check the device.
- CI: R CMD check on Windows/Linux/macOS with vignettes.

## Dev

`devtools::load_all()` · `devtools::document()` · `devtools::test()` · `devtools::check()`.
Needs `ens` installed (`install.packages("~/GitHub/ens", repos=NULL, type="source")`).
Commit work when complete. Co-author trailer: `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
