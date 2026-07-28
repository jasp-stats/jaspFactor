#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <http://www.gnu.org/licenses/>.
#

numberOfFactorsInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Revelle, W. (2018) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.")

  ready   <- length(options$variables) > 1
  # Handle dataset
  dataset <- .pcaAndEfaHandleData(dataset, options, ready)
  .pcaAndEfaDataCovarianceCheck(dataset, options, ready)

  if (ready)
    .pcaCheckErrors(dataset, options, method = "numberOfFactors")

  options(mc.cores = 1) # prevent the fa.parallel function using multiple cores by default

  container <- .nofContainer(jaspResults)

  # output functions
  .nofSummaryTable(          container, dataset, options, ready)
  .nofParallelAnalysisTable( container, dataset, options, ready)
  .nofScreePlot(             container, dataset, options, ready)
}


.nofContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["nofContainer"]])) {
    container <- jaspResults[["nofContainer"]]
  } else {
    container <- createJaspContainer()
    container$dependOn(c("variables", "variables.types", "dataType", "sampleSize", "naAction",
                         "baseDecompositionOn", "parallelAnalysisMethod", "setSeed", "seed"))
    jaspResults[["nofContainer"]] <- container
  }

  return(container)
}


# Results functions ----
.nofComputeParallel <- function(container, dataset, options) {

  # Return cached result if available
  if (!is.null(container[["parallelState"]]))
    return(container[["parallelState"]]$object)

  faMethod <- ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased", "pc", "fa")

  if (any(options$variables.types %in% c("ordinal", "nominal")) && options[["baseDecompositionOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- .efaComputePolyCorMatrix(container, dataset, options)
    if (is.null(polyTetraCor)) return(NULL)
    .setSeedJASP(options)
    parallelResult <- try(psych::fa.parallel(polyTetraCor, plot = FALSE, fa = faMethod, n.obs = nrow(dataset)))
  } else if (options[["dataType"]] == "varianceCovariance") {
    .setSeedJASP(options)
    parallelResult <- try(psych::fa.parallel(cov2cor(as.matrix(dataset)), plot = FALSE, fa = faMethod,
                                             n.obs = options[["sampleSize"]]))
  } else {
    .setSeedJASP(options)
    parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE, fa = faMethod))
  }

  if (isTryError(parallelResult)) {
    errmsg <- gettextf("Parallel analysis failed. Internal error message: %s", .extractErrorMessage(parallelResult))
    container$setError(errmsg)
    parallelResult <- NULL
  }

  container[["parallelState"]] <- createJaspState(parallelResult)
  return(parallelResult)
}


# Output functions ----
.nofSummaryTable <- function(container, dataset, options, ready) {
  if (!is.null(container[["summaryTable"]])) return()

  summaryTable <- createJaspTable(gettext("Suggested Number of Factors/Components"))
  summaryTable$dependOn("eigenvaluesAbove")
  summaryTable$position <- 1
  summaryTable$addColumnInfo(name = "method", title = gettext("Method"),           type = "string")
  summaryTable$addColumnInfo(name = "n",      title = gettext("Suggested number"), type = "integer")
  container[["summaryTable"]] <- summaryTable

  if (!ready || container$getError()) return()

  parallelResult <- .nofComputeParallel(container, dataset, options)
  if (is.null(parallelResult)) return()

  pcBased     <- options[["parallelAnalysisMethod"]] == "principalComponentBased"
  eigenValues <- if (pcBased) parallelResult$pc.values else parallelResult$fa.values
  eigType     <- if (pcBased) gettext("principal component") else gettext("factor")

  if (pcBased) {
    paLabel <- gettext("Parallel analysis (PC-based)")
    paCount <- max(1, parallelResult$ncomp)
  } else {
    paLabel <- gettext("Parallel analysis (FA-based)")
    paCount <- max(1, parallelResult$nfact)
  }

  summaryTable[["method"]] <- c(paLabel, gettextf("Eigenvalues above %s", options[["eigenvaluesAbove"]]))
  summaryTable[["n"]]      <- c(paCount, sum(eigenValues > options[["eigenvaluesAbove"]]))
  summaryTable$addFootnote(gettextf("Eigenvalue criterion based on %s eigenvalues.", eigType))
}


.nofParallelAnalysisTable <- function(container, dataset, options, ready) {
  if (!options[["parallelAnalysisTable"]] || !is.null(container[["parallelTable"]])) return()

  pcBased  <- options[["parallelAnalysisMethod"]] == "principalComponentBased"
  eigTitle <- if (pcBased) gettext("Real data component eigenvalues") else gettext("Real data factor eigenvalues")

  parallelTable <- createJaspTable(gettext("Parallel Analysis"))
  parallelTable$dependOn("parallelAnalysisTable")
  parallelTable$position <- 2
  parallelTable$addColumnInfo(name = "col",  title = "",                                          type = "string")
  parallelTable$addColumnInfo(name = "val1", title = eigTitle,                                    type = "number", format = "dp:3")
  parallelTable$addColumnInfo(name = "val2", title = gettext("Simulated data mean eigenvalues"),  type = "number", format = "dp:3")
  container[["parallelTable"]] <- parallelTable

  if (!ready || container$getError()) return()

  parallelResult <- .nofComputeParallel(container, dataset, options)
  if (is.null(parallelResult)) return()

  if (pcBased) {
    rowsName       <- gettext("Component")
    realDataEigen  <- parallelResult$pc.values
    resampledEigen <- parallelResult$pc.sim
    footnote       <- gettextf("'*' = %s should be retained. Results from PC-based parallel analysis.", rowsName)
  } else {
    rowsName       <- gettext("Factor")
    realDataEigen  <- parallelResult$fa.values
    resampledEigen <- parallelResult$fa.sim
    footnote       <- gettextf("'*' = %s should be retained. Results from FA-based parallel analysis.", rowsName)
  }

  retained <- realDataEigen - resampledEigen > 0
  firstCol <- ifelse(retained,
                     paste0(rowsName, " ", seq_along(realDataEigen), "*"),
                     paste(rowsName, seq_along(realDataEigen)))

  parallelTable[["col"]]  <- firstCol
  parallelTable[["val1"]] <- realDataEigen
  parallelTable[["val2"]] <- resampledEigen

  parallelTable$addFootnote(message = footnote)
}


.nofScreePlot <- function(container, dataset, options, ready) {
  if (!options[["screePlot"]] || !is.null(container[["screePlot"]])) return()

  scree <- createJaspPlot(title = gettext("Scree plot"), width = 480, height = 320)
  scree$dependOn(c("screePlot", "screePlotParallelAnalysisResults", "eigenvaluesAbove"))
  scree$position <- 3
  container[["screePlot"]] <- scree

  if (!ready || container$getError()) return()

  n_col   <- ncol(dataset)
  pcBased <- options[["parallelAnalysisMethod"]] == "principalComponentBased"
  xLabel  <- if (pcBased) gettext("Component") else gettext("Factor")

  # the real data eigenvalues always come from the same source as the x-axis label,
  # so toggling the simulated data overlay only hides that series
  parallelResult <- .nofComputeParallel(container, dataset, options)
  if (is.null(parallelResult)) return()

  realDataEigen <- if (pcBased) parallelResult$pc.values else parallelResult$fa.values

  if (options[["screePlotParallelAnalysisResults"]]) {
    resampledEigen <- if (pcBased) parallelResult$pc.sim else parallelResult$fa.sim
    evs <- c(realDataEigen, resampledEigen)
    tp  <- rep(c(gettext("Data"), gettext("Simulated data from parallel analysis")), each = n_col)
  } else { # do not display parallel analysis
    evs <- realDataEigen
    tp  <- rep(gettext("Data"), each = n_col)
  }

  df <- data.frame(
    id   = rep(seq_len(n_col), length(evs) / n_col),
    ev   = evs,
    type = tp
  )

  # basic scree plot
  plt <-
    ggplot2::ggplot(df, ggplot2::aes(x = id, y = ev, linetype = type, shape = type)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = xLabel, y = gettext("Eigenvalue")) +
    ggplot2::geom_hline(yintercept = options$eigenvaluesAbove)

  # dynamic function for point size:
  # the plot looks good with size 3 when there are 10 points (3 + log(10) - log(10) = 3)
  # with more points, the size will become logarithmically smaller until a minimum of
  # 3 + log(10) - log(200) = 0.004267726
  # with fewer points, they become bigger to a maximum of 3 + log(10) - log(2) = 4.609438
  pointsize <- 3 + log(10) - log(n_col)
  if (pointsize > 0) {
    plt <- plt + ggplot2::geom_point(na.rm = TRUE, size = max(0, 3 + log(10) - log(n_col)))
  }

  # add axis lines and better breaks
  plt <- plt +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::scale_x_continuous(breaks = seq(1:n_col))

  # theming with special legend thingy
  plt <- plt +
    ggplot2::theme(
      legend.position      = c(0.99, 0.95),
      legend.justification = c(1, 1),
      legend.text          = ggplot2::element_text(size = 12.5),
      legend.title         = ggplot2::element_blank(),
      legend.key.size      = ggplot2::unit(18, "pt")
    )

  scree$plotObject <- plt
}
