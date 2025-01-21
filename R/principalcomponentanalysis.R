#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <http://www.gnu.org/licenses/>.
#

principalComponentAnalysisInternal <- function(jaspResults, dataset, options, ...) {

  jaspResults$addCitation("Revelle, W. (2018) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.")

  # Read dataset
  dataset <- .pcaAndEfaReadData(dataset, options)
  ready   <- length(options$variables) > 1

  dataset <- .pcaAndEfaDataCovariance(dataset, options, ready)


  if (ready)
    .pcaCheckErrors(dataset, options)


  modelContainer <- .pcaModelContainer(jaspResults)

  # output functions
  .pcaGoodnessOfFitTable( modelContainer, dataset, options, ready)
  .pcaLoadingsTable(      modelContainer, dataset, options, ready)
  .pcaEigenTable(         modelContainer, dataset, options, ready)
  .pcaCorrTable(          modelContainer, dataset, options, ready)
  .pcaResidualTable(      modelContainer, dataset, options, ready)
  .parallelAnalysisTable( modelContainer, dataset, options, ready, name = "Component")
  .efaKMOtest(            modelContainer, dataset, options, ready)
  .efaBartlett(           modelContainer, dataset, options, ready)
  .efaMardia(             modelContainer, dataset, options, ready)
  .efaAntiImageCorrelation(modelContainer, dataset, options, ready)
  .pcaScreePlot(          modelContainer, dataset, options, ready)
  .pcaPathDiagram(        modelContainer, dataset, options, ready)

  # data saving
  .pcaAndEfaAddScoresToData(jaspResults, modelContainer, options, ready)

}

# Preprocessing functions ----
.pcaAndEfaReadData <- function(dataset, options) {

  # browser()
  if (!is.null(dataset)) return(dataset)

  if (options[["dataType"]] == "raw") {
    if (options[["naAction"]] == "listwise") {
      return(.readDataSetToEnd(columns.as.numeric = unlist(options$variables), exclude.na.listwise = unlist(options$variables)))
    } else {
      return(.readDataSetToEnd(columns.as.numeric = unlist(options$variables)))
    }
  } else { # if variance covariance matrix as input
    return(.readDataSetToEnd(all.columns = TRUE))
  }

}


.pcaAndEfaDataCovariance <- function(dataset, options, ready) {

  if (!ready) return()

  if (options[["dataType"]] == "raw") {
    return(dataset)
  }

  # possible data matrix?
  if ((nrow(dataset) != ncol(dataset)))
    .quitAnalysis(gettext("Input data does not seem to be a square matrix! Please check the format of the input data."))

  if (!all(dataset[lower.tri(dataset)] == t(dataset)[lower.tri(dataset)]))
    .quitAnalysis(gettext("Input data does not seem to be a symmetric matrix! Please check the format of the input data."))

  usedvars <- unlist(options[["variables"]])
  var_idx  <- match(usedvars, colnames(dataset))
  mat <- try(as.matrix(dataset[var_idx, var_idx]))
  if (inherits(mat, "try-error"))
    .quitAnalysis(gettext("All cells must be numeric."))

  .hasErrors(mat, type = "varCovMatrix", message='default', exitAnalysisIfErrors = TRUE)

  colnames(mat) <- rownames(mat) <- colnames(dataset)[var_idx]

  if (anyNA(mat)) {
    inds <- which(is.na(mat))
    mat <- mat[-inds, -inds]
    if (ncol(mat) < 3) {
      .quitAnalysis("Not enough valid columns to run this analysis")
    }
  }
  return(mat)
}


.pcaCheckErrors <- function(dataset, options) {

  customChecksPCAEFA <- list(
    function() {
      if (length(options$variables) > 0 && options$componentCountMethod == "manual" &&
          options$manualNumberOfComponents > length(options$variables)) {
        return(gettextf("Too many factors requested (%i) for the amount of included variables", options$manualNumberOfComponents))
      }
    },
    function() {
      if(nrow(dataset) < 3){
        return(gettextf("Not enough valid cases (%i) to run this analysis", nrow(dataset)))
      }
    },
    # check whether all row variance == 0
    function() {
      varianceZero <- 0
      for (i in 1:nrow(dataset)){
        if(sd(dataset[i,], na.rm = TRUE) == 0) varianceZero <- varianceZero + 1
      }
      if(varianceZero == nrow(dataset)){
        return(gettext("Data not valid: variance is zero in each row"))
      }
    },
    # Check for correlation anomalies
    function() {
      P <- ncol(dataset)
      # the checks below also fail when n < p but this provides a more accurate error message
      if (ncol(dataset) > nrow(dataset))
        return(gettext("Data not valid: there are more variables than observations"))

      # check whether a variable has too many missing values to compute the correlations
      Np <- colSums(!is.na(dataset))
      error_variables <- names(Np)[Np < P]
      if (length(error_variables) > 0) {
        return(gettextf("Data not valid: too many missing values in variable(s) %s.",
                        paste(error_variables, collapse = ", ")))
      }

      S <- cor(dataset)
      if (all(S == 1)) {
        return(gettext("Data not valid: all variables are collinear"))
      }
    },
    function() {
      if (ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
        return(gettext("Not more cases than number of variables. Is your data a variance-covariance matrix?"))
      }
    }
  )

  if (options[["dataType"]] == "raw") {
    error <- .hasErrors(dataset = dataset, type = c("infinity", "variance", "varCovData"), custom = customChecksPCAEFA,
                        exitAnalysisIfErrors = TRUE)
  }

  return()
}

.pcaModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("rotationMethod", "orthogonalSelector", "obliqueSelector", "variables", "componentCountMethod",
                              "eigenValuesAbove", "manualNumberOfComponents", "naAction", "analysisBasedOn",
                              "parallelAnalysisMethod", "dataType", "sampleSize"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}


# Results functions ----
.pcaComputeResults <- function(modelContainer, dataset, options, ready) {

  rotate <- options[[if (options[["rotationMethod"]] == "orthogonal") "orthogonalSelector" else "obliqueSelector"]]

  # see https://github.com/jasp-stats/jasp-test-release/issues/1500
  # GPArotation::GPFoblq does
  # Method <- paste("vgQ", method, sep = ".")
  # VgQ <- do.call(Method, append(list(L), methodArgs))
  # where Method is defined in psych... so this only works if you do library(psych) beforehand.
  # to work around this we temporarily assign this function to the global environment, if this rotation method is selected.
  if (rotate == "biquartimin") {
    tmp <- mget("vgQ.bimin", envir = .GlobalEnv, ifnotfound = NA)[[1L]]
    assign(x = "vgQ.bimin", value = psych::vgQ.bimin, envir = .GlobalEnv)
    on.exit({
      if (is.na(tmp)) {
        rm("vgQ.bimin", envir = .GlobalEnv)
      } else {
        # restore whatever was assigned before
        assign("vgQ.bimin", tmp, envir = .GlobalEnv)
      }
    })
  }
  corMethod <- switch(options[["analysisBasedOn"]],
                      "correlationMatrix" = "cor",
                      "covarianceMatrix" = "cov",
                      "polyTetrachoricCorrelationMatrix" = "mixed")
  pcaResult <- try(
    psych::principal(
      r        = dataset,
      nfactors = .pcaGetNComp(dataset, options, modelContainer),
      rotate   = rotate,
      scores   = TRUE,
      covar    = options$analysisBasedOn == "covarianceMatrix",
      cor      = corMethod,
      n.obs    = ifelse(options[["dataType"]] == "raw", NULL, options[["sampleSize"]])
    ))

  if (isTryError(pcaResult)) {
    errmsg <- gettextf("Estimation failed. Internal error message: %s", .extractErrorMessage(pcaResult))
    # when polychoric corr matrix is used, the warning generated here is useful to know for the user
    warns <- warnings()
    warnmsg <- warns[grep("polychoric", warns)]
    if (length(warnmsg) > 0) {
      errmsg <- paste(errmsg, "\n Warning in: ", warnmsg, names(warnmsg))
    }
    modelContainer$setError(errmsg)
  }

  modelContainer[["model"]] <- createJaspState(pcaResult)
  return(pcaResult)
}

.pcaGetNComp <- function(dataset, options, modelContainer) {

  if (options$componentCountMethod == "manual") return(options$manualNumberOfComponents)

  if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- psych::mixedCor(dataset)
    parallelResult <- try(psych::fa.parallel(polyTetraCor$rho,
                                 plot = FALSE,
                                 fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                             "pc", "fa"),
                                 n.obs = nrow(dataset)))
  } else {
    parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE,
                                             fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                                         "pc", "fa")))
  }

  if (options$componentCountMethod == "parallelAnalysis") {

    if (isTryError(parallelResult)) {
      errmsg <- gettextf("Parallel analysis failed. Internal error message: %s", .extractErrorMessage(parallelResult))
      modelContainer$setError(errmsg)
    }

    if (options$parallelAnalysisMethod == "principalComponentBased") {
      return(max(1, parallelResult$ncomp))
    } else { # parallel method is fa
      return(max(1, parallelResult$nfact))
    }
  }

  if (options$componentCountMethod == "eigenValues") {
    ncomp <- sum(parallelResult$pc.values > options$eigenValuesAbove)
    # I can use stop() because it's caught by the try and the message is put on
    # on the modelcontainer.
    if (ncomp == 0)
      .quitAnalysis( gettextf("No factors with an eigenvalue > %1$s. Maximum observed eigenvalue equals %2$s.",
                              options$eigenValuesAbove, round(max(parallelResult$fa.values), 3)))
    return(ncomp)
  }

}


# Output functions ----
.pcaGoodnessOfFitTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["goodnessOfFitTable"]])) return()

  goodnessOfFitTable <- createJaspTable(title = gettext("Chi-squared Test"))
  goodnessOfFitTable$addColumnInfo(name = "model", title = "",               type = "string")
  goodnessOfFitTable$addColumnInfo(name = "chisq", title = gettext("Value"), type = "number", format = "dp:3")
  goodnessOfFitTable$addColumnInfo(name = "df",    title = gettext("df"),    type = "integer")
  goodnessOfFitTable$addColumnInfo(name = "p",     title = gettext("p"),     type = "number", format = "dp:3;p:.001")
  goodnessOfFitTable$position <- 1

  modelContainer[["goodnessOfFitTable"]] <- goodnessOfFitTable

  if (!ready) return()

  pcaResults <- .pcaComputeResults(modelContainer, dataset, options)
  if (modelContainer$getError()) return()
  goodnessOfFitTable[["model"]] <- "Model"

  goodnessOfFitTable[["chisq"]] <- pcaResults$STATISTIC
  goodnessOfFitTable[["df"]]    <- pcaResults$dof
  goodnessOfFitTable[["p"]]     <- pcaResults$PVAL


  if (pcaResults$dof < 0)
    goodnessOfFitTable$addFootnote(message = gettext("Degrees of freedom below 0, model is unidentified."), symbol = gettext("<em>Warning:</em>"))
}


.pcaLoadingsTable <- function(modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["loadingsTable"]])) return()

  loadingsTable <- createJaspTable(gettext("Component Loadings"))
  loadingsTable$dependOn(c("loadingsDisplayLimit", "loadingsOrder"))
  loadingsTable$position <- 2
  loadingsTable$addColumnInfo(name = "var", title = "", type = "string")
  modelContainer[["loadingsTable"]] <- loadingsTable

  if (!ready || modelContainer$getError()) return()

  pcaResults <- modelContainer[["model"]][["object"]]
  loads <- loadings(pcaResults)

  coltitle <- if (options[["rotationMethod"]] == "orthogonal") "PC" else "RC"
  for (i in seq_len(ncol(loads))) {
    loadingsTable$addColumnInfo(name = paste0("c", i), title = paste0(coltitle, i), type = "number", format = "dp:3")
  }
  loadingsTable$addColumnInfo(name = "uni", title = gettext("Uniqueness"), type = "number", format = "dp:3")

  if (options[["rotationMethod"]] == "orthogonal" && options[["orthogonalSelector"]] == "none") {
    loadingsTable$addFootnote(message = gettext("No rotation method applied."))
  } else {
    loadingsTable$addFootnote(
      message = gettextf("Applied rotation method is %s.",
                         if (options[["rotationMethod"]] == "orthogonal") options[["orthogonalSelector"]] else options[["obliqueSelector"]])
    )
  }

  loadings <- unclass(loads)
  loadings[abs(loads) < options[["loadingsDisplayLimit"]]] <- NA_real_

  df <- cbind.data.frame(
    var = rownames(loads),
    as.data.frame(loadings),
    uni = pcaResults[["uniquenesses"]]
  )
  rownames(df) <- NULL
  colnames(df)[2:(1 + ncol(loads))] <- paste0("c", seq_len(ncol(loads)))

  # "sortByVariables" is the default output
  if (options[["loadingsOrder"]] == "sortBySize")
    df <- df[do.call(order, c(abs(df[2:(ncol(df) - 1)]), na.last = TRUE, decreasing = TRUE)), ]

  loadingsTable$setData(df)
  modelContainer[["loadingsTable"]] <- loadingsTable

}


.pcaEigenTable <- function(modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["eigenTable"]])) return()

  eigenTable <- createJaspTable(gettext("Component Characteristics"))
  eigenTable$addColumnInfo(name = "comp", title = "",                type = "string")

  # check if a rotation is used
  rotate <- options[[if (options[["rotationMethod"]] == "orthogonal") "orthogonalSelector" else "obliqueSelector"]]
  if (rotate != "none") {
    overTitleA <- gettext("Unrotated solution")
    overTitleB <- gettext("Rotated solution")

    eigenTable$addColumnInfo(name = "eigvU", title = gettext("Eigenvalue"),      type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "propU", title = gettext("Proportion var."), type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "cumpU", title = gettext("Cumulative"),      type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "eigvR", title = gettext("SumSq. Loadings"), type = "number", overtitle = overTitleB)
    eigenTable$addColumnInfo(name = "propR", title = gettext("Proportion var."), type = "number", overtitle = overTitleB)
    eigenTable$addColumnInfo(name = "cumpR", title = gettext("Cumulative"),      type = "number", overtitle = overTitleB)
  } else {
    eigenTable$addColumnInfo(name = "eigvU", title = gettext("Eigenvalue"),      type = "number")
    eigenTable$addColumnInfo(name = "propU", title = gettext("Proportion var."), type = "number")
    eigenTable$addColumnInfo(name = "cumpU", title = gettext("Cumulative"),      type = "number")
  }


  eigenTable$position <- 3

  modelContainer[["eigenTable"]] <- eigenTable

  if (!ready || modelContainer$getError()) return()

  pcaResults <- modelContainer[["model"]][["object"]]

  eigv <- pcaResults$values
  Vaccounted <- pcaResults[["Vaccounted"]]
  idx <- seq_len(pcaResults[["factors"]])

  eigenTable[["comp"]] <- paste("Component", idx)
  eigenTable[["eigvU"]] <- eigv[idx]
  eigenTable[["propU"]] <- eigv[1:pcaResults$factors] / sum(eigv)
  eigenTable[["cumpU"]] <- cumsum(eigv)[1:pcaResults$factors] / sum(eigv)
  if (rotate != "none") {
    eigenTable[["eigvR"]] <- Vaccounted["SS loadings", idx]
    eigenTable[["propR"]] <- Vaccounted["Proportion Var", idx]
    eigenTable[["cumpR"]] <- if (pcaResults[["factors"]] == 1L) Vaccounted["Proportion Var", idx] else Vaccounted["Cumulative Var", idx]
  }
}


.pcaCorrTable <- function(modelContainer, dataset, options, ready) {

  if (!options[["componentCorrelations"]] || !is.null(modelContainer[["correlationTable"]])) return()
  correlationTable <- createJaspTable(gettext("Component Correlations"))
  correlationTable$dependOn("componentCorrelations")
  correlationTable$addColumnInfo(name = "col", title = "", type = "string")
  correlationTable$position <- 4
  modelContainer[["correlationTable"]] <- correlationTable

  if (!ready || modelContainer$getError()) return()

  pcaResult <- modelContainer[["model"]][["object"]]

  if (pcaResult$factors == 1 || options$rotationMethod == "orthogonal") {
    # no factor correlation matrix when rotation specified uncorrelated factors!
    cors <- diag(pcaResult$factors)
  } else {
    cors <- zapsmall(pcaResult$Phi)
  }

  dims <- ncol(cors)

  correlationTable[["col"]] <- paste("Component", 1:dims)

  for (i in 1:dims) {
    thisname <- paste("Component", i)
    correlationTable$addColumnInfo(name = thisname, title = thisname, type = "number", format = "dp:3")
    correlationTable[[thisname]] <- cors[, i]
  }

}

.pcaResidualTable <- function(modelContainer, dataset, options, ready) {

  if (!options[["residualMatrix"]] || !is.null(modelContainer[["residualTable"]])) return()
  residualTable <- createJaspTable(gettext("Residual Matrix"))
  residualTable$dependOn("residualMatrix")
  residualTable$addColumnInfo(name = "col1", title = "", type = "string")
  residualTable$position <- 5
  modelContainer[["residualTable"]] <- residualTable

  if (!ready || modelContainer$getError()) return()

  pcaResult <- modelContainer[["model"]][["object"]]

  residuals <- pcaResult$residual
  cols <- ncol(residuals)
  residualTable[["col1"]] <- options[["variables"]] # fill the rows

  for (i in 1:cols) {
    value <- paste0("value", i)
    residualTable$addColumnInfo(name = value, title = options[["variables"]][i], type = "number", format = "dp:3")
    residualTable[[value]] <- residuals[, i]
  }

}


.pcaScreePlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["screePlot"]] || !is.null(modelContainer[["scree"]])) return()

  scree <- createJaspPlot(title = gettext("Scree plot"), width = 480, height = 320)
  scree$dependOn(c("screePlot", "screePlotParallelAnalysisResults", "parallelAnalysisMethod"))
  modelContainer[["scree"]] <- scree

  if (!ready || modelContainer$getError()) return()

  n_col <- ncol(dataset)

  if (options[["screePlotParallelAnalysisResults"]]) {

    if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
      polyTetraCor <- psych::mixedCor(dataset)
      parallelResult <- try(psych::fa.parallel(polyTetraCor$rho,
                                   plot = FALSE,
                                   fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                               "pc", "fa"),
                                   n.obs = nrow(dataset)))
    } else {
      parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE,
                                               fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                                           "pc", "fa")))
    }

    if (isTryError(parallelResult)) {
      errmsg <- gettextf("Screeplot not available. \nInternal error message: %s", .extractErrorMessage(parallelResult))
      scree$setError(errmsg)
      # scree$setError(.decodeVarsInMessage(names(dataset), errmsg))
      return()
    }

    if (options$componentCountMethod == "parallelAnalysis" && options$parallelAnalysisMethod == "factorBased") {
      evs <- c(parallelResult$fa.values, parallelResult$fa.sim)
    } else { # in all other cases we use the initial eigenvalues for the plot, aka the pca ones
      evs <- c(parallelResult$pc.values, parallelResult$pc.sim)
    }
    tp <- rep(c(gettext("Data"), gettext("Simulated data from parallel analysis")), each = n_col)

  } else { # do not display parallel analysis
    evs <- eigen(cor(dataset, use = "pairwise.complete.obs"), only.values = TRUE)$values
    tp <- rep(gettext("Data"), each = n_col)
  }

  df <- data.frame(
    id   = rep(seq_len(n_col), 2),
    ev   = evs,
    type = tp
  )
  # basic scree plot
  plt <-
    ggplot2::ggplot(df, ggplot2::aes(x = id, y = ev, linetype = type, shape = type)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = gettext("Component"), y = gettext("Eigenvalue")) +
    ggplot2::geom_hline(yintercept = options$eigenValuesAbove)


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
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      legend.position      = c(0.99, 0.95),
      legend.justification = c(1, 1),
      legend.text          = ggplot2::element_text(size = 12.5),
      legend.title         = ggplot2::element_blank(),
      legend.key.size      = ggplot2::unit(18, "pt")
    )

  scree$plotObject <- plt
  modelContainer[["scree"]] <- scree
}

.pcaPathDiagram <- function(modelContainer, dataset, options, ready){
  if (!options[["pathDiagram"]] || !is.null(modelContainer[["path"]])) return()

  # Create plot object
  n_var <- length(options$variables)
  path <- createJaspPlot(title = gettext("Path Diagram"), width = 480, height = ifelse(n_var < 2, 300, 1 + 299 * (n_var / 5)))
  path$dependOn(c("pathDiagram", "loadingsDisplayLimit"))
  modelContainer[["path"]] <- path
  if (!ready || modelContainer$getError()) return()

  # Get result info
  pcaResult <- modelContainer[["model"]][["object"]]
  LY <- as.matrix(loadings(pcaResult))
  TE <- diag(pcaResult$uniqueness)
  PS <- pcaResult$r.scores

  # Variable names
  xName   <- ifelse(options$rotationMethod == "orthogonal" && options$orthogonalSelector == "none", "PC", "RC")
  factors <- paste0(xName, seq_len(ncol(LY)))
  labels  <- rownames(LY)

  # Number of variables:
  nFactor    <- length(factors)
  nIndicator <- length(labels)
  nTotal     <- nFactor + nIndicator

  # Make layout:
  # For each manifest, find strongest loading:
  strongest <- apply(abs(LY), 1, which.max)
  ord       <- order(strongest)

  # Reshuffle labels and LY:
  labels <- labels[ord]
  LY     <- LY[ord,]

  # Edgelist:
  # Factor loadings
  E_loadings <- data.frame(
    from   = rep(labels, nFactor),
    to     = rep(factors, each = nIndicator),
    weight = c(LY),
    stringsAsFactors = FALSE
  )

  # Residuals:
  E_resid <- data.frame(
    from   = labels,
    to     = labels,
    weight = diag(TE)
  )

  # Factor correlations:
  E_cor <- data.frame(
    from   = c(factors[col(PS)]),
    to     = c(factors[row(PS)]),
    weight = c(PS),
    stringsAsFactors = FALSE
  )
  E_cor <- E_cor[E_cor$from != E_cor$to, ]

  # Combine everything:
  edge_df <- rbind(E_loadings, E_resid, E_cor)

  # Make the layout:
  sq <- function(x) seq(-1, 1, length.out = x + 2)[-c(1, x + 2)]

  layout_mat <- cbind(
    c(rep(-1, nFactor), rep(1, nIndicator)),
    c(sq(nFactor),      sq(nIndicator))
  )

  # Compute curvature of correlations:
  # Numeric edgelist:
  E_cor_numeric <- cbind(match(E_cor$from, factors), match(E_cor$to, factors))

  # Compute distance:
  dist <- abs(layout_mat[E_cor_numeric[,1], 2] - layout_mat[E_cor_numeric[,2], 2])
  min <- 2
  max <- 8

  # Scale to max:
  dist <- min + dist / (max(dist)) * (max - min)
  if (length(unique(dist)) == 1) {
    dist[] <- mean(c(max, min))
  }

  # Scale to plot width:
  Scale <- sqrt(path$width^2 + path$height^2) / sqrt(480^2 + 300^2)
  dist <- 1 / Scale * dist

  # Curvature:
  curve <- c(rep(0, nrow(E_loadings)), rep(0, nrow(E_resid)), dist)

  # Edge connectpoints:
  ECP <- matrix(NA, nrow(edge_df), 2)
  ECP[nrow(E_loadings) + nrow(E_resid) + seq_len(nrow(E_cor)), 1:2] <- 1.5 * pi
  ECP[seq_len(nrow(E_loadings)), 1] <- 1.5 * pi

  # Loop rotation:
  loopRotation <- 0.5*pi

  # bidirectional:
  bidir <- c(rep(FALSE, nrow(E_loadings) + nrow(E_resid)), rep(TRUE, nrow(E_cor)))

  # Shape:
  shape <- c(rep("circle", nFactor), rep("rectangle", nIndicator))

  # Size:
  size1 <- c(rep(12, nFactor), rep(30, nIndicator))
  size2 <- c(rep(12, nFactor), rep( 7, nIndicator))

  # Plot:
  label.scale.equal <- c(rep(1, nFactor),rep(2, nIndicator))

  path$plotObject <- jaspBase:::.suppressGrDevice(qgraph::qgraph(
    input               = edge_df,
    layout              = layout_mat,
    directed            = TRUE,
    bidirectional       = bidir,
    residuals           = TRUE,
    residScale	        = 10,
    labels              = c(factors,labels),
    curve               = curve,
    curveScale          = FALSE,
    edgeConnectPoints   = ECP,
    loopRotation        = loopRotation,
    shape               = shape,
    vsize               = size1,
    vsize2              = size2,
    label.scale.equal   = label.scale.equal,
    residScale          = 2,
    mar                 = c(5,10,5,12),
    normalize           = FALSE,
    label.fill.vertical = 0.75,
    cut                 = options$loadingsDisplayLimit,
    bg                  = "transparent"
  ))

}


.pcaAndEfaAddScoresToData <- function(jaspResults, modelContainer, options, ready) {

  if (!ready ||
      !is.null(jaspResults[["addedScoresContainer"]]) ||
      modelContainer$getError() ||
      !options[["addScores"]] ||
      options[["dataType"]] == "varianceCovariance")
  {
    return()
  }

  colNamesR <- paste0(options[["addedScoresPrefix"]], "_", seq_len(length(options$variables)))

  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = modelContainer, options = c("addScores", "addedScoresPrefix", "naAction"))

  scores <- modelContainer[["model"]][["object"]][["scores"]]

  for (ii in seq_len(ncol(scores))) {

    colNameR <- colNamesR[ii]

    if (jaspBase:::columnExists(colNameR) && !jaspBase:::columnIsMine(colNameR)) {
      .quitAnalysis(gettextf("Column name %s already exists in the dataset", colNameR))
    }

    container[[colNameR]] <- jaspBase::createJaspColumn(colNameR)
    if (options[["naAction"]] == "pairwise") {
      container[[colNameR]]$setScale(scores[, ii])
    } else { # for listwise we need to identify the complete cases
      # so we need to temporarily load the raw data with the NAs
      dataTmp <- .readDataSetToEnd(columns.as.numeric = unlist(options$variables))
      scoresTmp <- rep(NA, nrow(dataTmp))
      scoresTmp[complete.cases(dataTmp)] <- scores[, ii]
      container[[colNameR]]$setScale(scoresTmp)

    }

  }

  jaspResults[["addedScoresContainer"]] <- container

  # check if there are previous colNames that are not needed anymore and delete the cols
  oldNames <- jaspResults[["createdColumnNames"]][["object"]]
  newNames <- colNamesR[1:ii]
  if (!is.null(oldNames)) {
    noMatch <- which(!(oldNames %in% newNames))
    if (length(noMatch) > 0) {
      for (i in 1:length(noMatch)) {
        jaspBase:::columnDelete(oldNames[noMatch[i]])
      }
    }
  }

  # save the created col names
  jaspResults[["createdColumnNames"]] <- createJaspState(newNames)


  return()

}


