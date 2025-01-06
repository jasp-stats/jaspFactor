#
# Copyright (C) 2013-2018 University of Amsterdam
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

exploratoryFactorAnalysisInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Revelle, W. (2018) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.")

  # Read dataset
  dataset <- .pcaAndEfaReadData(dataset, options)
  ready   <- length(options$variables) > 1
  dataset <- .pcaAndEfaDataCovariance(dataset, options, ready)

  if (ready)
    .efaCheckErrors(dataset, options)

  options(mc.cores = 1) # prevent the fa.parallel function using mutlitple cores by default

  modelContainer <- .efaModelContainer(jaspResults)

  # output functions
  .efaKMOtest(           modelContainer, dataset, options, ready)
  .efaBartlett(          modelContainer, dataset, options, ready)
  .efaMardia(            modelContainer, dataset, options, ready)
  .efaGoodnessOfFitTable(modelContainer, dataset, options, ready)
  .efaLoadingsTable(     modelContainer, dataset, options, ready)
  .efaStructureTable(    modelContainer, dataset, options, ready)
  .efaEigenTable(        modelContainer, dataset, options, ready)
  .efaCorrTable(         modelContainer, dataset, options, ready)
  .efaAdditionalFitTable(modelContainer, dataset, options, ready)
  .efaResidualTable(     modelContainer,          options, ready)
  .parallelAnalysisTable(modelContainer, dataset, options, ready, name = "Factor")
  .efaScreePlot(         modelContainer, dataset, options, ready)
  .efaPathDiagram(       modelContainer, dataset, options, ready)

  # data saving
  .commonAddScoresToData(jaspResults, modelContainer, options, ready)
}


.efaCheckErrors <- function(dataset, options) {
  customChecksEFA <- list(
    function() {
      if (length(options$variables) > 0 && options$factorCountMethod == "manual" &&
          options$manualNumberOfFactors > length(options$variables)) {
        return(gettextf("Too many factors requested (%i) for the amount of included variables", options$manualNumberOfFactors))
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
    error <- .hasErrors(dataset = dataset, type = c("infinity", "variance", "varCovData"), custom = customChecksEFA,
                        exitAnalysisIfErrors = TRUE)
  }


  return()
}

.efaModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("rotationMethod", "orthogonalSelector", "obliqueSelector", "variables", "factorCountMethod",
                              "eigenValuesAbove", "manualNumberOfFactors", "naAction", "analysisBasedOn", "factoringMethod",
                              "parallelAnalysisMethod", "dataType", "sampleSize"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}


# Results functions ----
# Modification here: added "cor" argument to the fa function.
# If analysisBasedOn == polyTetrachoricCorrelationMatrix, the fa will be performed computing a tetrachoric or polychoric correlation matrix,
# depending on the number of response categories of the ordinal variables.
.efaComputeResults <- function(modelContainer, dataset, options, ready) {
  corMethod <- switch(options[["analysisBasedOn"]],
                      "correlationMatrix" = "cor",
                      "covarianceMatrix" = "cov",
                      "polyTetrachoricCorrelationMatrix" = "mixed")

  factoringMethod <- switch(options[["factoringMethod"]],
                            "minimumResidual"         = "minres",
                            "maximumLikelihood"       = "ml",
                            "principalAxis"           = "pa",
                            "ordinaryLeastSquares"    = "ols",
                            "weightedLeastSquares"    = "wls",
                            "generalizedLeastSquares" = "gls",
                            "minimumChiSquare"        = "minchi",
                            "minimumRank"             = "minrank"
                            )
  efaResult <- try(
    psych::fa(
      r        = dataset,
      nfactors = .efaGetNComp(dataset, options),
      rotate   = ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector),
      scores   = TRUE,
      covar    = options$analysisBasedOn == "covarianceMatrix",
      cor      = corMethod,
      fm       = factoringMethod,
      n.obs    = ifelse(options[["dataType"]] == "raw", NULL, options[["sampleSize"]])
    )
  )

  if (isTryError(efaResult)) {
    errtxt <- .extractErrorMessage(efaResult)
    errcodes <- c("L-BFGS-B needs finite values of 'fn'", "missing value where TRUE/FALSE needed",
                  "reciprocal condition number = 0", "infinite or missing values in 'x'")
    if (errtxt %in% errcodes) {
      errmsg <- gettextf("Estimation failed. Internal error message: %1$s. %2$s", .extractErrorMessage(efaResult),
                         "Try basing the analysis on a different type of matrix or change the factoring method.")
    } else {
      errmsg <- gettextf("Estimation failed. Internal error message: %s", .extractErrorMessage(efaResult))
    }

    modelContainer$setError(errmsg)
  }

# Modification here: if the estimation of the polychoric/tetrachoric correlation matrix fails with this specific error,
# JASP replaces the internal error message with a more informative one.
  if (isTryError(efaResult) && options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix" &&
      (.extractErrorMessage(efaResult) == "missing value where TRUE/FALSE needed" ||
       .extractErrorMessage(efaResult) == "attempt to set 'rownames' on an object with no dimensions")) {
    errmsgPolyCor <- gettextf(
      "Unfortunately, the estimation of the polychoric/tetrachoric correlation matrix failed.
      This might be due to a small sample size or variables not containing all response categories.
      Internal error message: %s", .extractErrorMessage(efaResult))
    modelContainer$setError(errmsgPolyCor)
  }

  modelContainer[["model"]] <- createJaspState(efaResult)
  return(efaResult)
}

.efaGetNComp <- function(dataset, options) {
  if (options$factorCountMethod == "manual") return(options$manualNumberOfFactors)

  if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- psych::mixedCor(dataset)
    .setSeedJASP(options)

    parallelResult <- try(psych::fa.parallel(polyTetraCor$rho,
                                 plot = FALSE,
                                 fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                             "pc", "fa"),
                                 n.obs = nrow(dataset)))
  }
  else {
    .setSeedJASP(options)

    parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE,
                                             fa = ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                                                         "pc", "fa")))
  }
  if (isTryError(parallelResult)) return(1)
  if (options$factorCountMethod == "parallelAnalysis") {
    return(ifelse(options[["parallelAnalysisMethod"]] == "principalComponentBased",
                  max(1, parallelResult$ncomp), max(1, parallelResult$nfact)))
  }
  if (options$factorCountMethod == "eigenValues") {
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
.efaKMOtest <- function(modelContainer, dataset, options, ready) {

  if (!options[["kaiserMeyerOlkinTest"]] || !is.null(modelContainer[["kmoTable"]])) return()

  kmoTable <- createJaspTable(gettext("Kaiser-Meyer-Olkin Test"))
  kmoTable$dependOn("kaiserMeyerOlkinTest")
  kmoTable$addColumnInfo(name = "col", title = "", type = "string")
  kmoTable$addColumnInfo(name = "val", title = "MSA", type = "number", format = "dp:3")
  kmoTable$position <- -1
  modelContainer[["kmoTable"]] <- kmoTable

  if (!ready) return()

  # Modification here:
  # If a polychoric/tetrachoric-correlation-based FA is requested, then compute the KMO values
  # based on said correlation matrix:
  # else: analysis carries on as usual
  if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- psych::mixedCor(dataset)
    kmo <- psych::KMO(polyTetraCor$rho)
  } else {
    if (options[["dataType"]] == "raw")
      kmo <- psych::KMO(dataset)
    else
      kmo <- psych::KMO(cov2cor(as.matrix(dataset)))
  }

  kmoTable[["col"]] <- c(gettext("Overall MSA\n"), names(kmo$MSAi))
  kmoTable[["val"]] <- c(kmo$MSA, kmo$MSAi)
}

.efaBartlett <- function(modelContainer, dataset, options, ready) {
  if (!options[["bartlettTest"]] || !is.null(modelContainer[["bartlettTable"]])) return()

  bartlettTable <- createJaspTable(gettext("Bartlett's Test"))
  bartlettTable$dependOn("bartlettTest")
  bartlettTable$addColumnInfo(name = "chisq", title = "\u03a7\u00b2", type = "number", format = "dp:3")
  bartlettTable$addColumnInfo(name = "df",    title = gettext("df"), type = "number", format = "dp:3")
  bartlettTable$addColumnInfo(name = "pval",  title = gettext("p"), type = "number", format = "dp:3;p:.001")
  bartlettTable$position <- 0
  modelContainer[["bartlettTable"]] <- bartlettTable

  if (!ready) return()


  if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- psych::mixedCor(dataset)
    bar <- psych::cortest.bartlett(polyTetraCor$rho, n = nrow(dataset))
  } else {
    if (options[["dataType"]] == "raw")
      bar <- psych::cortest.bartlett(dataset)
    else
      bar <- psych::cortest.bartlett(cov2cor(as.matrix(dataset)))
  }

  bartlettTable[["chisq"]] <- bar[["chisq"]]
  bartlettTable[["df"]]    <- bar[["df"]]
  bartlettTable[["pval"]]  <- bar[["p.value"]]
}


.efaMardia <- function(modelContainer, dataset, options, ready) {

  if (!options[["mardiaTest"]] || !is.null(modelContainer[["mardiaTable"]])) return()

  mardiaTable <- createJaspTable(gettext("Mardia's Test of Multivariate Normality"))
  mardiaTable$dependOn("mardiaTest")
  mardiaTable$addColumnInfo(name = "tests", title =  "", type = "number", format = "dp:3")
  mardiaTable$addColumnInfo(name = "coefs", title =  gettext("Value"), type = "number", format = "dp:3")
  mardiaTable$addColumnInfo(name = "statistics", title =  gettext("Statistic"), type = "number", format = "dp:3")
  mardiaTable$addColumnInfo(name = "dfs", title =  gettext("df"), type = "integer")
  mardiaTable$addColumnInfo(name = "pval",  title = gettext("p"), type = "number", format = "dp:3;p:.001")
  mardiaTable$position <- 0.5
  modelContainer[["mardiaTable"]] <- mardiaTable

  if (!ready) return()

  mar <- try(psych::mardia(dataset, plot = FALSE), silent = TRUE)
  if (isTryError(mar)) {
    errmsg <- gettextf("Mardia test failed. Internal error message: %s", .extractErrorMessage(mar))
    mardiaTable$setError(errmsg)
    return()
  }

  mardiaHead <- c("Skewness","Small Sample Skewness", "Kurtosis")

  # dfs of the skewness coefficients are calculated via Mardia's formula (1970);
  # package psych doesn't seem to provide them
  k <- length(dataset)
  mardiadfs <- (k * (k + 1) * (k + 2)) / 6

  mardiaTable[["tests"]] <- mardiaHead
  mardiaTable[["coefs"]] <- c(mar[["b1p"]], mar[["b1p"]], mar[["b2p"]])
  mardiaTable[["statistics"]] <- c(mar[["skew"]], mar[["small.skew"]], mar[["kurtosis"]])
  mardiaTable[["dfs"]] <- c(mardiadfs, mardiadfs)
  mardiaTable[["pval"]] <- c(mar[["p.skew"]], mar[["p.small"]], mar[["p.kurt"]])

   mardiaTable$addFootnote(message = gettext("The statistic for skewness is assumed to be Chi^2 distributed and the statistic for kurtosis standard normal."))
}

.efaGoodnessOfFitTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["goodnessOfFitTable"]])) return()

  goodnessOfFitTable <- createJaspTable(title = gettext("Chi-squared Test"))
  goodnessOfFitTable$addColumnInfo(name = "model", title = "",                 type = "string")
  goodnessOfFitTable$addColumnInfo(name = "chisq", title = gettext("Value"),   type = "number", format = "dp:3")
  goodnessOfFitTable$addColumnInfo(name = "df",    title = gettext("df"),      type = "integer")
  goodnessOfFitTable$addColumnInfo(name = "p",     title = gettext("p"),       type = "number", format = "dp:3;p:.001")
  goodnessOfFitTable$position <- 1

  modelContainer[["goodnessOfFitTable"]] <- goodnessOfFitTable

  if (!ready) return()

  efaResults <- .efaComputeResults(modelContainer, dataset, options)
  if (modelContainer$getError()) return()

  goodnessOfFitTable[["model"]] <- "Model"
  goodnessOfFitTable[["chisq"]] <- efaResults$STATISTIC
  goodnessOfFitTable[["df"]]    <- efaResults$dof
  goodnessOfFitTable[["p"]]     <- efaResults$PVAL

  if (efaResults$dof < 0)
    goodnessOfFitTable$addFootnote(message = gettext("Degrees of freedom below 0, model is unidentified."), symbol = gettext("<em>Warning:</em>"))
}

.efaLoadingsTable <- function(modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["loadingsTable"]]))
    return()

  loadingsTable <- createJaspTable(gettext("Factor Loadings"))
  loadingsTable$dependOn(c("loadingsDisplayLimit", "factorLoadingsOrder"))
  loadingsTable$position <- 2

  loadingsTable$addColumnInfo(name = "var", title = "", type = "string")

  if (!ready || modelContainer$getError()) {
    modelContainer[["loadingsTable"]] <- loadingsTable
    return()
  }

  efaResults <- modelContainer[["model"]][["object"]]
  loads <- loadings(efaResults)

  for (i in seq_len(ncol(loads))) {
    loadingsTable$addColumnInfo(name = paste0("c", i), title = gettextf("Factor %i", i), type = "number", format = "dp:3")
  }

  loadingsTable$addColumnInfo(name = "uni", title = gettext("Uniqueness"), type = "number", format = "dp:3")

  if (options[["rotationMethod"]] == "orthogonal" && options[["orthogonalSelector"]] == "none") {
    loadingsTable$addFootnote(message = gettext("No rotation method applied."))
  } else {
    loadingsTable$addFootnote(message = gettextf("Applied rotation method is %s.", options[[if(options[["rotationMethod"]] == "orthogonal") "orthogonalSelector" else "obliqueSelector"]]))
  }

  loadings <- unclass(loads)
  loadings[abs(loads) < options[["loadingsDisplayLimit"]]] <- NA_real_

  df <- cbind.data.frame(
    var = rownames(loads),
    as.data.frame(loadings),
    uni = efaResults[["uniquenesses"]]
  )
  rownames(df) <- NULL
  colnames(df)[2:(1 + ncol(loads))] <- paste0("c", seq_len(ncol(loads)))

  # "sortByVariables" is the default output
  if (options[["factorLoadingsOrder"]] == "sortByFactorSize")
    df <- df[do.call(order, c(abs(df[2:(ncol(df) - 1)]), na.last = TRUE, decreasing = TRUE)), ]

  loadingsTable$setData(df)
  modelContainer[["loadingsTable"]] <- loadingsTable

}

.efaStructureTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["factorStructure"]] || !is.null(modelContainer[["structureTable"]])) return()
  structureTable <- createJaspTable(gettext("Factor Loadings (Structure Matrix)"))
  structureTable$dependOn(c("loadingsDisplayLimit", "factorStructure"))
  structureTable$position <- 2.5
  structureTable$addColumnInfo(name = "var", title = "", type = "string")
  modelContainer[["structureTable"]] <- structureTable

  if (!ready || modelContainer$getError()) return()

  efaResults <- modelContainer[["model"]][["object"]]

  if (options$rotationMethod == "orthogonal" && options$orthogonalSelector == "none") {
    structureTable$addFootnote(message = gettext("No rotation method applied."))
  } else {
    structureTable$addFootnote(
      message = gettextf("Applied rotation method is %s.", ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector))
    )
  }

  loads <- efaResults$Structure
  structureTable[["var"]] <- rownames(loads)

  for (i in 1:ncol(loads)) {
    # fix weird "all true" issue
    if (all(abs(loads[, i]) < options$loadingsDisplayLimit)) {
      structureTable$addColumnInfo(name = paste0("c", i), title = gettextf("Factor %i", i), type = "string")
      structureTable[[paste0("c", i)]] <- rep("", nrow(loads))
    } else {
      structureTable$addColumnInfo(name = paste0("c", i), title = gettextf("Factor %i", i), type = "number", format = "dp:3")
      structureTable[[paste0("c", i)]] <- ifelse(abs(loads[, i]) < options$loadingsDisplayLimit, NA, loads[ ,i])
    }
  }
}

.efaEigenTable <- function(modelContainer, dataset, options, ready) {

  if (!is.null(modelContainer[["eigenTable"]])) return()

  eigenTable <- createJaspTable(gettext("Factor Characteristics"))
  eigenTable$addColumnInfo(name = "comp", title = "",                         type = "string")

  # if a rotation is used, the table needs more columns
  rotate <- options[[if (options[["rotationMethod"]] == "orthogonal") "orthogonalSelector" else "obliqueSelector"]]
  eigenTable$addColumnInfo(name = "eigen", title = gettext("Eigenvalues"),  type = "number")
  if (rotate != "none") {
    overTitleA <- gettext("Unrotated solution")
    overTitleB <- gettext("Rotated solution")
    eigenTable$addColumnInfo(name = "sslU", title = gettext("SumSq. Loadings"),  type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "propU", title = gettext("Proportion var."), type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "cumpU", title = gettext("Cumulative"),      type = "number", overtitle = overTitleA)
    eigenTable$addColumnInfo(name = "sslR", title = gettext("SumSq. Loadings"),  type = "number", overtitle = overTitleB)
    eigenTable$addColumnInfo(name = "propR", title = gettext("Proportion var."), type = "number", overtitle = overTitleB)
    eigenTable$addColumnInfo(name = "cumpR", title = gettext("Cumulative"),      type = "number", overtitle = overTitleB)
  } else {
    eigenTable$addColumnInfo(name = "sslU", title = gettext("SumSq. Loadings"),  type = "number")
    eigenTable$addColumnInfo(name = "propU", title = gettext("Proportion var."), type = "number")
    eigenTable$addColumnInfo(name = "cumpU", title = gettext("Cumulative"),      type = "number")
  }

  eigenTable$position <- 3

  modelContainer[["eigenTable"]] <- eigenTable

  if (!ready || modelContainer$getError()) return()

  efaResults <- modelContainer[["model"]][["object"]]


  eigv <- efaResults$values
  eigv_init <- efaResults$e.values
  Vaccounted <- efaResults[["Vaccounted"]]
  idx <- seq_len(efaResults[["factors"]])

  eigenTable[["eigen"]] <- eigv_init[idx]
  eigenTable[["comp"]] <- paste("Factor", idx)
  eigenTable[["sslU"]] <- eigv[idx]
  eigenTable[["propU"]] <- eigv[idx] / sum(eigv_init)
  eigenTable[["cumpU"]] <- cumsum(eigv)[idx] / sum(eigv_init)
  if (rotate != "none") {
    eigenTable[["sslR"]] <- Vaccounted["SS loadings", idx]
    eigenTable[["propR"]] <- Vaccounted["Proportion Var", idx]
    eigenTable[["cumpR"]] <- if (efaResults[["factors"]] == 1L) Vaccounted["Proportion Var", idx] else Vaccounted["Cumulative Var", idx]
  }
}

.efaCorrTable <- function(modelContainer, dataset, options, ready) {

  if (!options[["factorCorrelations"]] || !is.null(modelContainer[["correlationTable"]])) return()
  correlationTable <- createJaspTable(gettext("Factor Correlations"))
  correlationTable$dependOn("factorCorrelations")
  correlationTable$addColumnInfo(name = "col", title = "", type = "string")
  correlationTable$position <- 4
  modelContainer[["correlationTable"]] <- correlationTable

  if (!ready || modelContainer$getError()) return()

  efaResult <- modelContainer[["model"]][["object"]]

  if (efaResult$factors == 1 || options$rotationMethod == "orthogonal") {
    # no factor correlation matrix when rotation specified uncorrelated factors!
    cors <- diag(efaResult$factors)
  } else {
    cors <- zapsmall(efaResult$Phi)
  }

  dims <- ncol(cors)

  correlationTable[["col"]] <- paste("Factor", 1:dims)

  for (i in 1:dims) {
    thisname <- paste("Factor", i)
    correlationTable$addColumnInfo(name = thisname, title = thisname, type = "number", format = "dp:3")
    correlationTable[[thisname]] <- cors[,i]
  }

}

.efaAdditionalFitTable <- function(modelContainer, dataset, options, ready) {

  if (!options[["fitIndices"]] || !is.null(modelContainer[["fitTable"]])) return()
  fitTable <- createJaspTable(gettext("Additional fit indices"))
  fitTable$dependOn("fitIndices")
  fitTable$addColumnInfo(name = "RMSEA",   title = gettext("RMSEA"), type = "number", format = "dp:3")
  fitTable$addColumnInfo(name = "RMSEAci", title = gettextf("RMSEA 90%% confidence"),   type = "string")
  fitTable$addColumnInfo(name = "SRMR",    title = gettext("SRMR"),   type = "number", format = "dp:3")
  fitTable$addColumnInfo(name = "TLI",     title = gettext("TLI"),   type = "number", format = "dp:3")
  fitTable$addColumnInfo(name = "CFI",     title = gettext("CFI"),   type = "number", format = "dp:3")
  fitTable$addColumnInfo(name = "BIC",     title = gettext("BIC"),   type = "number", format = "dp:3")

  fitTable$position <- 4.5
  modelContainer[["fitTable"]] <- fitTable

  if (!ready || modelContainer$getError()) return()

  efaResults <- modelContainer[["model"]][["object"]]

  # store in obj
  rmsealo <- if (is.null(efaResults$RMSEA[2])) "." else round(efaResults$RMSEA[2], 3)
  rmseahi <- if (is.null(efaResults$RMSEA[3])) "." else round(efaResults$RMSEA[3], 3)

  fitTable[["RMSEA"]]   <- if (is.null(efaResults$RMSEA[1])) NA  else efaResults$RMSEA[1]
  fitTable[["RMSEAci"]] <- paste(rmsealo, "-", rmseahi)
  fitTable[["TLI"]]     <- if (is.null(efaResults$TLI))      NA  else efaResults$TLI
  fitTable[["BIC"]]     <- if (is.null(efaResults$BIC))      NA  else efaResults$BIC

  # SRMR, see Hu and Bentler (1998)
  if (is.null(efaResults$Phi)) {
    phi <- diag(efaResults$factors)
  } else {
    phi <- efaResults$Phi
  }

  impl <- efaResults$loadings[] %*% phi %*% t(efaResults$loadings[]) + diag(diag(efaResults$residual))
  cmat_data <- efaResults$r
  lobs <-  cmat_data[!lower.tri(cmat_data)]
  limp <-  impl[!lower.tri(impl)]
  fitTable[["SRMR"]] <- sqrt(mean((limp - lobs)^2))

  # CFI (see Hu & Bentler, 1998)
  d0 <- efaResults$null.chisq - efaResults$null.dof
  dmod <- efaResults$STATISTIC - efaResults$dof
  fitTable[["CFI"]] <- 1 - (max(dmod, 0) / max(d0, dmod, 0))

}


.efaResidualTable <- function(modelContainer, options, ready) {

  if (!options[["residualMatrix"]] || !is.null(modelContainer[["residualTable"]])) return()
  residualTable <- createJaspTable(gettext("Residual Matrix"))
  residualTable$dependOn("residualMatrix")
  residualTable$addColumnInfo(name = "col1", title = "", type = "string")
  residualTable$position <- 5
  modelContainer[["residualTable"]] <- residualTable

  if (!ready || modelContainer$getError()) return()

  efaResult <- modelContainer[["model"]][["object"]]

  residuals <- efaResult$residual
  cols <- ncol(residuals)
  residualTable[["col1"]] <- options[["variables"]] # fill the rows

  for (i in 1:cols) {
    value <- paste0("value", i)
    residualTable$addColumnInfo(name = value, title = options[["variables"]][i], type = "number", format = "dp:3")
    residualTable[[value]] <- residuals[, i]
  }

}


.parallelAnalysisTable <- function(modelContainer, dataset, options, ready, name = "Factor") {
  if (!options[["parallelAnalysisTable"]] || !is.null(modelContainer[["parallelTable"]])) return()

  if (!ready || modelContainer$getError()) return()

  if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
    polyTetraCor <- psych::mixedCor(dataset)
    .setSeedJASP(options)

    parallelResult <- try(psych::fa.parallel(polyTetraCor$rho,
                                             plot = FALSE,
                                             fa = ifelse(options[["parallelAnalysisTableMethod"]] == "principalComponentBased",
                                                         "pc", "fa"),
                                             n.obs = nrow(dataset)))
  } else {
    .setSeedJASP(options)

    parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE,
                                             fa = ifelse(options[["parallelAnalysisTableMethod"]] == "principalComponentBased",
                                                         "pc", "fa")))
  }

  if (options$parallelAnalysisTableMethod == "principalComponentBased") {
    eigTitle <- gettext("Real data component eigenvalues")
    rowsName <- gettext(name)
    RealDataEigen <- parallelResult$pc.values
    ResampledEigen <- parallelResult$pc.sim
    footnote <- gettextf("'*' = %s should be retained. Results from PC-based parallel analysis.", name)
  } else { # parallelAnalysisMethod is FA
    eigTitle <- gettext("Real data factor eigenvalues")
    rowsName <- gettext(name)
    RealDataEigen <- parallelResult$fa.values
    ResampledEigen <- parallelResult$fa.sim
    footnote <- gettextf("'*' = %s should be retained. Results from FA-based parallel analysis.", name)
  }

  parallelTable <- createJaspTable(gettext("Parallel Analysis"))
  parallelTable$dependOn(c("parallelAnalysisTable", "parallelAnalysisTableMethod"))
  parallelTable$addColumnInfo(name = "col", title = "", type = "string")

  parallelTable$addColumnInfo(name = "val1", title = eigTitle, type = "number", format = "dp:3")
  parallelTable$addColumnInfo(name = "val2", title = gettext("Simulated data mean eigenvalues"), type = "number", format = "dp:3")
  parallelTable$position <- 6
  modelContainer[["parallelTable"]] <- parallelTable


  efaResult <- modelContainer[["model"]][["object"]]
  PAs <- zapsmall(as.matrix(data.frame(RealDataEigen, ResampledEigen), fix.empty.names = FALSE))
  forasterisk <- data.frame(applyforasterisk = RealDataEigen - ResampledEigen)
  dims <- nrow(PAs)

  firstcol <- data.frame()
  for (i in 1:dims) {
    if (forasterisk$applyforasterisk[i] > 0) {
      firstcol <- rbind(firstcol, paste(rowsName, " ", i,"*", sep = ""))
    } else {
      firstcol <- rbind(firstcol, paste(rowsName, i))
    }
  }

  parallelTable[["col"]] <- firstcol[[1]]
  parallelTable[["val1"]] <- c(RealDataEigen)
  parallelTable[["val2"]] <- c(ResampledEigen)

  parallelTable$addFootnote(message = footnote)
}


.efaScreePlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["screePlot"]] || !is.null(modelContainer[["scree"]])) return()

  scree <- createJaspPlot(title = "Scree plot", width = 480, height = 320)
  scree$dependOn(c("screePlot", "screePlotParallelAnalysisResults", "parallelAnalysisMethod"))
  scree$position <- 8
  modelContainer[["scree"]] <- scree

  if (!ready || modelContainer$getError()) return()

  n_col <- ncol(dataset)

  if (options[["screePlotParallelAnalysisResults"]]) {

    # Modification here:
    # if "analysisBasedOn = mixed", parallel analysis here will be based on the polychoric/tetrachoric
    # correlation matrix.

    if (options[["analysisBasedOn"]] == "polyTetrachoricCorrelationMatrix") {
      polyTetraCor <- psych::mixedCor(dataset)
      .setSeedJASP(options)

      parallelResult <- try(psych::fa.parallel(polyTetraCor$rho,
                                   plot = FALSE,
                                   fa = ifelse(options[["parallelAnalysisTableMethod"]] == "principalComponentBased",
                                               "pc", "fa"),
                                   n.obs = nrow(dataset)))
    } else {
      .setSeedJASP(options)

      parallelResult <- try(psych::fa.parallel(dataset, plot = FALSE,
                                               fa = ifelse(options[["parallelAnalysisTableMethod"]] == "principalComponentBased",
                                                           "pc", "fa")))
    }
    if (isTryError(parallelResult)) {
      errmsg <- gettextf("Screeplot not available. \nInternal error message: %s", .extractErrorMessage(parallelResult))
      scree$setError(errmsg)
      # scree$setError(.decodeVarsInMessage(names(dataset), errmsg))
      return()
    }

    if (options$parallelAnalysisTableMethod == "factorBased") {
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
    ggplot2::labs(x = gettext("Factor"), y = gettext("Eigenvalue")) +
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

.efaPathDiagram <- function(modelContainer, dataset, options, ready){
  if (!options[["pathDiagram"]] || !is.null(modelContainer[["path"]])) return()

  # Create plot object
  n_var <- length(options$variables)
  path <- createJaspPlot(title = gettext("Path Diagram"), width = 480, height = ifelse(n_var < 2, 300, 1 + 299 * (n_var / 5)))
  path$dependOn(c("pathDiagram", "loadingsDisplayLimit"))
  path$position <- 7
  modelContainer[["path"]] <- path
  if (!ready || modelContainer$getError()) return()

  # Get result info
  efaResult <- modelContainer[["model"]][["object"]]
  LY <- as.matrix(loadings(efaResult))
  TE <- diag(efaResult$uniqueness)
  PS <- efaResult$r.scores


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
    from   = rep(factors, each = nIndicator),
    to     = rep(labels, nFactor),
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
  if (efaResult$factors == 1) {
    E_cor <- data.frame(from = NULL, to = NULL, weight = NULL)
  } else {
    E_cor <- data.frame(
      from   = c(factors[col(PS)]),
      to     = c(factors[row(PS)]),
      weight = c(PS),
      stringsAsFactors = FALSE
    )
    E_cor <- E_cor[E_cor$from != E_cor$to, ]
  }

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
  ECP[seq_len(nrow(E_loadings)), 2] <- 1.5 * pi

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


.commonAddScoresToData <- function(jaspResults, modelContainer, options, ready) {

  if (!ready ||
      !is.null(jaspResults[["addedScoresContainer"]]) ||
      modelContainer$getError() ||
      !options[["addScores"]])
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

