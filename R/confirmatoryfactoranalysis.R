# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


confirmatoryFactorAnalysisInternal <- function(jaspResults, dataset, options, ...) {

  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")


  # Preprocess options
  options <- .cfaPreprocessOptions(options)

  # Read dataset
  dataset <- .cfaReadData(dataset, options)

  # Error checking
  errors <- .cfaCheckErrors(dataset, options)

  # covariance matrix
  dataset <- .cfaDataCovariance(dataset, options)

  # Main table / model
  cfaResult <- .cfaComputeResults(jaspResults, dataset, options, errors)

  # Output tables
  .cfaContainerMain(   jaspResults, options, cfaResult          ) # Main table container
  .cfaTableMain(       jaspResults, options, cfaResult          ) # Main table with fit info
  .cfaTableFitMeasures(jaspResults, options, cfaResult          ) # Additional fit indices
  .cfaTableKMO(        jaspResults, options, cfaResult          ) # Kaiser-Meyer-Olkin test.
  .cfaTableBartlett(   jaspResults, options, cfaResult          ) # Bartlett's test of sphericity
  .cfaTableRsquared(   jaspResults, options, cfaResult          ) # R-squared of indicators
  .cfaTableParEst(     jaspResults, options, cfaResult          ) # Parameter estimates tables
  .cfaTableModIndices( jaspResults, options, cfaResult          ) # Modification Indices
  .cfaTableImpliedCov( jaspResults, options, cfaResult          ) # Implied Covariance matrix
  .cfaTableResCov(     jaspResults, options, cfaResult          ) # Residual Covariance Matrix
  .cfaTableAve(        jaspResults, options, cfaResult          ) # Average variance explained table
  .cfaTableHtmt(       jaspResults, options, cfaResult, dataset ) # Heterotrait monotrait
  # weirdly enough I cannot find the exact sample cov matrix in the lavaan output
  .cfaTableReliability(jaspResults, options, cfaResult          ) # Reliability


  # Output plots
  .cfaInitPlots( jaspResults, options, cfaResult)      # Create plots container
  .cfaPlotPath(  jaspResults, options, cfaResult)      # Path plot(s) showing model
  .cfaPlotMisfit(jaspResults, options, cfaResult)      # Plot residual correlations

  # Output model syntax
  .cfaSyntax(jaspResults, options, dataset, cfaResult) # Output model syntax to user

  # add factor scores to data
  .cfaAddScoresToData(jaspResults, options, cfaResult, dataset)

  return()
}

# Preprocessing functions ----
.cfaReadData <- function(dataset, options) {

  if (!is.null(dataset)) return(dataset)

  # NOTE: The GUI does not yet allow for putting the same variable in different factors.
  # if the same variable is used twice but with a different type then this would
  # crash the R code. However, since this is not possible yet, this should be okay for now
  vars  <- unlist(lapply(options[["factors"]], `[[`, "indicators"),       use.names = FALSE)

  if (length(vars) == 0)
    return(data.frame())

  duplicateVars <- duplicated(vars)
  vars  <- vars[!duplicateVars]

  if (options[["dataType"]] == "raw") {
    # make sure on the qml side that groupVar is indeed a nominal variable
    groupVar <- if (options[["group"]] == "") NULL else options[["group"]]
    dataset <- .readDataSetToEnd(columns = c(vars, groupVar))

  } else {

    dataset <- .readDataSetToEnd(all.columns = TRUE)
  }


  return(dataset)

}

.cfaPreprocessOptions <- function(options) {
  # Remove empty factors
  facwoindicators <- sapply(options$factors, function(f) length(f$indicators)) == 0
  options$factors <- options$factors[!facwoindicators]

  # sofacwoindicators   <- sapply(options$secondOrder, function(f) length(f$indicators)) == 0
  # options$secondOrder <- options$secondOrder[!sofacwoindicators]
  if (length(options$secondOrder) > 0) {
    options$secondOrder <- list(list(indicators = options$secondOrder, name = "SecondOrder", title = gettext("Second-Order")))
  }
  return(options)
}


.cfaCheckErrors <- function(dataset, options) {

  # Number of variables in the factors
  nVarsPerFactor <- unlist(lapply(options$factors, function(x) setNames(length(x$indicators), x$title)))
  if (all(nVarsPerFactor == 0)) return("No variables")
  if (any(nVarsPerFactor == 1)) jaspBase:::.quitAnalysis(gettext("The model could not be estimated. Ensure that factors have at least 2 observed variables."))

  # TODO (tj), call error handling before all the options get screwed around so we can simply do `for (factor in options[["secondOrder"]])`
  if (length(options[["secondOrder"]]) > 0)
    for (factor in options[["secondOrder"]][[1]][["indicators"]])
      if (!factor %in% names(nVarsPerFactor) || nVarsPerFactor[factor] <= 0)
        jaspBase:::.quitAnalysis(gettext("The model could not be estimated. A factor with less than 2 variables was added in Second-Order."))

  vars <- unique(unlist(lapply(options$factors, function(x) x$indicators)))

  if (options[["dataType"]] == "raw") {

    # possible cov matrix:
    if (ncol(dataset) > 0 && !nrow(dataset) > ncol(dataset)) {
      .quitAnalysis(gettext("Not more cases than number of variables. Is your data a variance-covariance matrix?"))
    }

    if (options$group == "") {


      .hasErrors(dataset[, vars], type = 'varCovData', exitAnalysisIfErrors = TRUE,
                 varCovData.corFun = stats::cov)

    } else {

      .hasErrors(dataset, type = "factorLevels", factorLevels.target = options$group,
                 factorLevels.amount = '< 2', exitAnalysisIfErrors = TRUE)

      for (group in levels(dataset[[options$group]])) {

        idx <- dataset[[options$group]] == group

        if (any(sapply(dataset[, vars], is.ordered))) {
          .hasErrors(dataset[idx, vars], type = 'varCovData', exitAnalysisIfErrors = TRUE,
                     varCovData.corFun = lavaan::lavCor)
        } else {
          .hasErrors(dataset[idx, vars], type = 'varCovData', exitAnalysisIfErrors = TRUE,
                     varCovData.corFun = stats::cov)
        }
      }
    }
  }

  return(NULL)
}

.cfaDataCovariance <- function(dataset, options) {


  if (options[["dataType"]] == "raw") {
    return(dataset)
  }

  vars  <- unlist(lapply(options[["factors"]], `[[`, "indicators"), use.names = FALSE)

  # are there any variables specified at all?
  if (length(vars) == 0)
    return(data.frame())

  # possible data matrix?
  if ((nrow(dataset) != ncol(dataset)))
    .quitAnalysis(gettext("Input data does not seem to be a square matrix! Please check the format of the input data."))

  if (!all(dataset[lower.tri(dataset)] == t(dataset)[lower.tri(dataset)]))
    .quitAnalysis(gettext("Input data does not seem to be a symmetric matrix! Please check the format of the input data."))

  duplicateVars <- duplicated(vars)
  usedvars  <- vars[!duplicateVars]
  var_idx  <- match(usedvars, colnames(dataset))
  mat <- try(as.matrix(dataset[var_idx, var_idx]))
  if (inherits(mat, "try-error"))
    .quitAnalysis(gettext("All cells must be numeric."))

  if (options[["group"]] != "") .quitAnalysis(gettext("Grouping variable not supported for covariance matrix input"))

  if (options[["meanStructure"]]) .quitAnalysis(gettext("Mean structure not supported for covariance matrix input"))

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


.translateFactorNames <- function(factor_name, options, back = FALSE) {
  # make dictionary
  fac_names    <- vapply(options$factors, function(x) x$name, "name")
  fac_titles   <- vapply(options$factors, function(x) x$title, "title")
  sofac_names  <- vapply(options$secondOrder, function(x) x$name, "name")
  sofac_titles <- vapply(options$secondOrder, function(x) x$title, "title")
  fnames  <- c(fac_names, sofac_names)
  ftitles <- c(fac_titles, sofac_titles)
  # translate
  if (back) {
    idx <- vapply(factor_name, function(n) which(ftitles == n), 0L, USE.NAMES = FALSE)
    return(fnames[idx])
  } else {
    idx <- vapply(factor_name, function(n) which(fnames == n), 0L, USE.NAMES = FALSE)
    return(ftitles[idx])
  }
}

.cfaIsReady <- function(options) {
  # are all residual covariances pairs fully specified?
  for (rescov in options[["residualsCovarying"]]) {
    if ("" %in% rescov) return(FALSE)
  }

  return(TRUE)
}


# Results functions ----
.cfaComputeResults <- function(jaspResults, dataset, options, errors) {
  if (!is.null(errors) && errors == "No variables" || !.cfaIsReady(options)) return()

  if (!is.null(jaspResults[["stateCFAResult"]])) return(jaspResults[["stateCFAResult"]]$object)

  cfaResult <- list()

  cfaResult[["spec"]] <- .cfaCalcSpecs(dataset, options)
  # Recalculate the model

  modObj <- .optionsToCFAMod(options, dataset, cfaResult)
  mod <- modObj$model
  cfaResult[["model"]] <- mod
  cfaResult[["model_simple"]] <- modObj$simple_model
  geq <- .CFAInvariance(options)
  if (options$group == "") grp <- NULL else grp <- options$group

  if (anyNA(dataset)) {
    naAction <- switch(
      options$naAction,
      "twoStageRobust" = "robust.two.stage",
      "twoStage"       = "two.stage",
      options$naAction)
  } else {
    naAction <- "listwise"
  }

  # define estimator from options
  estimator = switch(options[["estimator"]],
                     "default"                         = "default",
                     "maximumLikelihood"               = "ML",
                     "generalizedLeastSquares"         = "GLS",
                     "weightedLeastSquares"            = "WLS",
                     "unweightedLeastSquares"          = "ULS",
                     "diagonallyWeightedLeastSquares"  = "DWLS"
  )

  if (options[["dataType"]] == "raw") {
    dt <- dataset
    sampCov <- NULL
    sampCovN <- NULL
  } else {
    dt <- NULL
    sampCov <- dataset
    sampCovN <- options[["sampleSize"]]
  }
  cfaResult[["lav"]] <- try(lavaan::lavaan(
    model           = mod,
    data            = dt,
    sample.cov      = sampCov,
    sample.nobs     = sampCovN,
    group           = grp,
    group.equal     = geq,
    meanstructure   = options$meanStructure,
    se              = cfaResult[["spec"]]$se,
    std.lv          = options$modelIdentification == "factorVariance",
    auto.fix.first  = options$modelIdentification == "markerVariable",
    orthogonal      = options$factorsUncorrelated,
    int.ov.free     = (options$interceptsFixedToZero == "latent" || options$interceptsFixedToZero == "meanManifest"),
    int.lv.free     = (options$interceptsFixedToZero == "manifest" || options$interceptsFixedToZero == "meanManifest"),
    effect.coding   = ifelse(options$modelIdentification == "effectsCoding", TRUE,
                             ifelse(options$interceptsFixedToZero == "meanManifest", "intercepts", FALSE)),
    auto.fix.single = TRUE,
    auto.var        = TRUE,
    auto.cov.lv.x   = TRUE,
    auto.th         = TRUE,
    auto.delta      = TRUE,
    auto.cov.y      = TRUE,
    mimic           = options$packageMimiced,
    estimator       = options[["estimator"]],
    missing         = naAction
  ))

  # are there ordered variables in the data?
  cfaResult[["orderedVariables"]] <- any(sapply(dataset, is.ordered))

  # Quit analysis on error
  if (inherits(cfaResult[["lav"]], "try-error")) {
    if (!options[["estimator"]] %in% c("default", "ML") && options[["naAction"]] == "fiml") {
      jaspBase:::.quitAnalysis(gettext("FIML missing data handling only available with ML-type estimators"))
    }

    if (options[["estimator"]] == "generalizedLeastSquares" && options[["seType"]] == "robust") {
      jaspBase::.quitAnalysis(gettext("Robust standard errors are not available with the GLS estimator. Try changing the standard error method in the 'Advanced' settings to fit the model."))
    }

    err <- attr(cfaResult[["lav"]], "condition")$message
    if(grepl("not available in the categorical", err)){
      if(grepl("ml", err))
        errMissingMethod <- "FIML"
      if(grepl("two.stage", err))
        errMissingMethod <- "Two-stage"
      if(grepl("robust.two.stage", err))
        errMissingMethod <- "Robust two-stage"
      err <- gettextf("Missing data handling '%s' is not supported for categorical data. \n Please select another method under 'Missing data handling' within the 'Advanced' options tab",
                      errMissingMethod)
    }
    jaspBase:::.quitAnalysis(gettextf("The model could not be estimated. Error message: \n\n %s", err))
  }

  admissible <- .withWarnings(lavaan:::lav_object_post_check(cfaResult[["lav"]]))

  if (!admissible$value) {
    jaspBase:::.quitAnalysis(gettextf("The model is not admissible: %s", admissible$warnings[[1]]$message))
  }

  if (!cfaResult[["lav"]]@optim$converged) {
    jaspBase:::.quitAnalysis(gettext("The model could not be estimated due to nonconvergence."))
  }

  if (cfaResult[["lav"]]@test[[1]]$df < 0) {
    jaspBase:::.quitAnalysis(gettext("The model could not be estimated: No degrees of freedom left."))
  }


  # Bootstrapping with interruptible progress bar
  if (cfaResult[["spec"]]$bootstrap) {
    type <- switch(options[["standardized"]],
                   "all" = "std.all",
                   "latentVariables" = "std.lv",
                   "noExogenousCovariates" = "std.nox")

    if (options[["dataType"]] == "varianceCovariance") {
      .quitAnalysis(gettext("Bootstrapping is not available for variance-covariance matrix input."))
    }
    cfaResult[["lav"]] <- jaspSem::lavBootstrap(cfaResult[["lav"]], options$bootstrapSamples,
                                                standard = options[["standardized"]] != "none", typeStd = type)
  }

  # Save cfaResult as state so it's available even when opts don't change
  jaspResults[["stateCFAResult"]] <- createJaspState(cfaResult)
  jaspResults[["stateCFAResult"]]$dependOn(c(
    "factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification",
    "factorsUncorrelated", "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples",
    "group", "invarianceTesting", "interceptsFixedToZero", "standardized", "dataType", "sampleSize"
  ))

  return(cfaResult)
}

.withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

.cfaCalcSpecs <- function(dataset, options) {
  spec <- list()
  spec$variables <- unlist(lapply(options$factors, function(x) x$indicators))
  spec$latents   <- vapply(options$factors,        function(x) x$name, "names")
  if (length(options$secondOrder) > 0) {
    spec$soIndics  <- .translateFactorNames(options$secondOrder[[1]]$indicators, options, back = TRUE)
  }
  if (options$seType == "bootstrap") {
    spec$se <- "standard"
    spec$bootstrap <- TRUE
  } else {
    if (options$seType == "robust") {
      if (options[["dataType"]] == "varianceCovariance") {
        .quitAnalysis(gettext("Robust standard errors are not available for variance-covariance matrix input."))
      }
      spec$se <- "robust.sem"
    } else {
      spec$se <- options$seType
    }
    spec$bootstrap <- FALSE
  }
  return(spec)
}

.optionsToCFAMod <- function(options, dataset, cfaResult, base64 = TRUE) {
  gv <- options$group
  if (!base64) .v <- identity

  vars    <- options$factors
  latents <- cfaResult[["spec"]]$latents
  labels  <- list()
  # add extra output here because the Htmt needs a model syntax without grouping labels
  labels_simp <- list()

  fo <- gettext("# Factors")
  fo_simp <- gettext("# Factors")
  for (i in 1:length(vars)) {
    pre <- paste0("\n", latents[i], " =~ ")
    len <- length(vars[[i]]$indicators)
    labelledvars <- character(len)
    labels[[i]] <- list()
    labelledvars_simp <- character(len)
    labels_simp[[i]] <- list()
    for (j in 1:len) {
      if (nchar(options$group) == 0 || options$invarianceTesting !="configural") {
        labels[[i]][[j]]  <- paste0("lambda_", i, "_", j)
        labelledvars[j] <- paste0("lambda_", i, "_", j, "*", vars[[i]]$indicators[j])
      } else { # grouping variable present and configural invarianceTesting
        # we need a vector with different labels per group for lavaan
        n_levels <- length(unique(na.omit(dataset[[options$group]])))
        tmp_labels <- paste0("lambda_", i, "_", j, "_", seq(n_levels))
        labels[[i]][[j]] <- tmp_labels
        labelledvars[j] <- paste0("c(", paste0(tmp_labels, collapse = ","), ")", "*", vars[[i]]$indicators[j])
      }
      # give the simple model always since that is needed for the HTMT
      labels_simp[[i]][[j]]  <- paste0("lambda_", i, "_", j)
      labelledvars_simp[j] <- paste0("lambda_", i, "_", j, "*", vars[[i]]$indicators[j])
    }
    fo <- paste0(fo, pre, paste0(labelledvars, collapse = " + "))
    fo_simp <- paste0(fo_simp, pre, paste0(labelledvars_simp, collapse = " + "))
  }


  if (!is.null(cfaResult[["spec"]]$soIndics)) {
    facs    <- cfaResult[["spec"]]$soIndics
    lenvars <- length(vars)

    so  <- "# Second-order factor"
    pre <- "\nSecondOrder =~ "
    len <- length(facs)
    labelledfacs <- character(len)
    labels[[lenvars + 1]] <- list()
    for (j in 1:len) {
      # the normal case, either no grouping or no configural invarianceTesting
      if (nchar(options$group) == 0 || options$invarianceTesting !="configural") {
        labels[[lenvars + 1]][[j]] <- paste0("gamma_1_", j)
        labelledfacs[j] <- paste0("gamma_1_", j, "*", facs[j])
      } else { # grouping variable present and configural invarianceTesting
        # we need a vector with different labels per group for lavaan
        tmp_labels <- paste0("gamma_1_", j, "_", seq(n_levels))
        labels[[lenvars + 1]][[j]] <- tmp_labels
        labelledfacs[j] <- paste0("c(", paste0(tmp_labels, collapse = ","), ")", "*", facs[j])
      }

    }

    so <- paste0(so, pre, paste0(labelledfacs, collapse = " + "))
  } else {
    so <- NULL
  }

  if (length(options$residualsCovarying) > 0) {
    rc <- "# Residual Correlations"
    for (rcv in options$residualsCovarying) {
      rc <- paste0(rc, "\n", rcv[1], " ~~ ", rcv[2])
    }
  } else {
    rc <- NULL
  }


  return(list(model = paste0(c(fo, so, rc), collapse = "\n\n"), simple_model = fo_simp))
}

.CFAInvariance <- function(options) {
  if (options$invarianceTesting == "") return("")
  switch(options$invarianceTesting,
         "configural" = return(""),
         "metric"     = return("loadings"),
         "scalar"     = return(c("loadings", "intercepts")),
         "strict"     = return(c("loadings", "intercepts", "residuals", "residual.covariances")),
         "structural" = return(c("loadings", "intercepts", "residuals", "residual.covariances",
                                 "means", "lv.variances", "lv.covariances"))
  )
}

# Output functions ----
.cfaContainerMain <- function(jaspResults, options, cfaResult) {
  # Create main container
  jaspResults[["maincontainer"]] <- createJaspContainer(gettext("Model fit"), position = 1)
  jaspResults[["maincontainer"]]$dependOn(c(
    "factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification",
    "factorsUncorrelated", "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples",
    "group", "invarianceTesting", "interceptsFixedToZero", "dataType", "sampleSize"
  ))
}

.cfaTableMain <- function(jaspResults, options, cfaResult) {
  # Main table
  # prepare table
  jaspResults[["maincontainer"]][["cfatab"]] <- maintab <- createJaspTable(gettext("Chi-square test"))
  maintab$dependOn(optionsFromObject = jaspResults[["maincontainer"]])

  maintab$addColumnInfo(name = "mod",    title = gettext("Model"), type = "string")
  maintab$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2",   type = "number", format = "dp:3")
  maintab$addColumnInfo(name = "df",     title = gettext("df"),    type = "integer")
  maintab$addColumnInfo(name = "pvalue", title = gettext("p"),     type = "number", format = "dp:3;p:.001")

  # add data
  if (is.null(cfaResult)) {
    maintab[["mod"]]    <- c(gettext("Baseline model"), gettext("Factor model"))
    maintab[["chisq"]]  <- c(           "."           ,           "."          )
    maintab[["df"]]     <- c(           "."           ,           "."          )
    maintab[["pvalue"]] <- c(           "."           ,           "."          )
  } else {
    # check the fit options
    fitOptions <- lavaan::inspect(cfaResult[["lav"]], what = "options")

    # get the model fit
    fitMeasures <- lavaan::fitmeasures(cfaResult[["lav"]])

    footnote <- ""
    if (cfaResult[["orderedVariables"]]) {
      # when the estimator is not default lavaan does not use the robust test
      maintab[["mod"]]    <- c(gettext("Baseline model"), gettext("Factor model"))
      maintab[["chisq"]]  <- fitMeasures[c("baseline.chisq.scaled", "chisq.scaled")]
      maintab[["df"]]     <- fitMeasures[c("baseline.df.scaled", "df.scaled")]
      maintab[["pvalue"]] <- c(NA, fitMeasures["pvalue.scaled"])

      if (options[["seType"]] == "standard") {
        footnote <- gettextf("%s You may consider changing the standard error method to 'robust'.", footnote)
      }

    } else {
      maintab[["mod"]]    <- c(gettext("Baseline model"), gettext("Factor model"))
      maintab[["chisq"]]  <- fitMeasures[c("baseline.chisq", "chisq")]
      maintab[["df"]]     <- fitMeasures[c("baseline.df", "df")]
      maintab[["pvalue"]] <- c(NA, fitMeasures["pvalue"])
    }

    if (options[["estimator"]] == "default") {
      footnote <- gettextf("%1$s The estimator is %2$s. The test statistic is %3$s.",
                           footnote, fitOptions$estimator, fitOptions$test)
    }

    if (options[["seType"]] == "default") {
      footnote <- gettextf("%1$s The standard error method is %2$s.", footnote, fitOptions$se)
    }

    maintab$addFootnote(footnote)

  }
  return()
}

.cfaTableKMO <- function(jaspResults, options, cfaResult) {
  if (!options$kaiserMeyerOlkinTest || !is.null(jaspResults[["maincontainer"]][["kmo"]])) return()

  jaspResults[["maincontainer"]][["kmo"]] <- tabkmo <- createJaspTable(gettext("Kaiser-Meyer-Olkin (KMO) test"))
  tabkmo$addColumnInfo(name = "indicator", title = "Indicator", type = "string")
  tabkmo$dependOn("kaiserMeyerOlkinTest")
  if (is.null(cfaResult)) return()

  cov_implied <- lavaan::fitted(cfaResult[["lav"]])
  cov_resids <- lavaan::resid(cfaResult[["lav"]])

  if (options$group != "") {
    group_names <- names(cov_implied)
    indicator_names <- colnames(cov_implied[[1]]$cov)
    indicator_names <- c(indicator_names, "Overall")
    tabkmo[["indicator"]] <- indicator_names
    for (group in group_names) {
      tabkmo$addColumnInfo(name = group, title = group, overtitle = "MSA", type = "number" )
      kmo_result <- psych::KMO(stats::cov2cor(cov_implied[[group]]$cov + cov_resids[[group]]$cov))
      tabkmo[[group]] <- c(kmo_result$MSAi, kmo_result$MSA)
    }
  } else {
    indicator_names <- colnames(cov_implied$cov)
    indicator_names <- c(indicator_names, "Overall")
    tabkmo[["indicator"]] <- indicator_names
    tabkmo$addColumnInfo(name = "value", title = "MSA", type = "number" )
    kmo_result <- psych::KMO(stats::cov2cor(cov_implied$cov + cov_resids$cov))
    tabkmo[["value"]] <- c(kmo_result$MSAi, kmo_result$MSA)
  }
}

.cfaTableBartlett <- function(jaspResults, options, cfaResult) {
  if (!options$bartlettTest || !is.null(jaspResults[["maincontainer"]][["bartlett"]])) return()

  jaspResults[["maincontainer"]][["bartlett"]] <- tabbartlett <- createJaspTable(gettext("Bartlett's test of sphericity"))
  tabbartlett$dependOn("bartlettTest")
  if (is.null(cfaResult)) return()

  cov_implied <- lavaan::fitted(cfaResult[["lav"]])
  cov_resids  <- lavaan::resid(cfaResult[["lav"]])

  if (options$group != "") {
    group_names <- names(cov_implied)
    for (group in seq_along(group_names)) {
      tabbartlett$addColumnInfo(name = paste0("chisq", group),  title = "\u03a7\u00b2", overtitle = group_names[group], type = "number" )
      tabbartlett$addColumnInfo(name = paste0("df", group),     title = "df",           overtitle = group_names[group], type = "integer")
      tabbartlett$addColumnInfo(name = paste0("pvalue", group), title = "p",            overtitle = group_names[group], type = "pvalue" )

      ns <- lavaan::lavInspect(cfaResult[["lav"]], what = "nobs")
      bartlett_result <- psych::cortest.bartlett(stats::cov2cor(cov_implied[[group_names[group]]]$cov + cov_resids[[group_names[group]]]$cov), ns[group])
      tabbartlett[[paste0("chisq", group)]]  <- bartlett_result$chisq
      tabbartlett[[paste0("df", group)]]     <- bartlett_result$df
      tabbartlett[[paste0("pvalue", group)]] <- bartlett_result$p.value
    }
  } else {
    tabbartlett$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number" )
    tabbartlett$addColumnInfo(name = "df",     title = "df",           type = "integer")
    tabbartlett$addColumnInfo(name = "pvalue", title = "p",            type = "pvalue" )

    n <- lavaan::lavInspect(cfaResult[["lav"]], what = "nobs")
    bartlett_result <- psych::cortest.bartlett(stats::cov2cor(cov_implied$cov + cov_resids$cov), n)
    tabbartlett[["chisq"]]  <- bartlett_result$chisq
    tabbartlett[["df"]]    <- bartlett_result$df
    tabbartlett[["pvalue"]] <- bartlett_result$p.value

  }
}

.cfaTableRsquared <- function(jaspResults, options, cfaResult) {

  if (!options$rSquared || !is.null(jaspResults[["maincontainer"]][["rSquared"]])) return()

  jaspResults[["maincontainer"]][["rSquared"]] <- tabr2 <- createJaspTable(gettext("R-Squared"))
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  tabr2$setExpectedSize(rows = 1, cols = 1)
  tabr2$dependOn("rSquared")

  if (is.null(cfaResult)) return()

  r2res <- lavaan::inspect(cfaResult[["lav"]], "r2")
  facNames <- cfaResult[["spec"]]$latents

  if (options$group != "") {
    # add columns with Rsq overtitle
    varnames <- names(r2res[[1]])
    fac_idx  <- varnames %in% facNames
    varnames[!fac_idx] <- varnames[!fac_idx]
    varnames[fac_idx]  <- .translateFactorNames(varnames[fac_idx], options)
    tabr2[["__var__"]] <- varnames
    lvls <- names(r2res)
    for (lvl in lvls) {
      tabr2$addColumnInfo(name = lvl, title = lvl, overtitle = "R\u00B2", type = "number", format = "sf:4;dp:3")
      tabr2[[lvl]] <- r2res[[lvl]]
    }
  } else {
    tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
    varnames <- names(r2res)
    fac_idx  <- varnames %in% facNames
    varnames[!fac_idx] <- varnames[!fac_idx]
    varnames[fac_idx]  <- .translateFactorNames(varnames[fac_idx], options)
    tabr2[["__var__"]] <- varnames
    tabr2[["rsq"]]     <- r2res
  }
}

.cfaTableFitMeasures <- function(jaspResults, options, cfaResult) {
  if (!options$fitMeasures || !is.null(jaspResults[["maincontainer"]][["fits"]])) return()
  jaspResults[["maincontainer"]][["fits"]] <- fitms <- createJaspContainer(gettext("Additional fit measures"))
  fitms$dependOn("fitMeasures")

  # Fit indices
  fitms[["indices"]] <- fitin <- createJaspTable(gettext("Fit indices"))
  fitin$addColumnInfo(name = "index", title = gettext("Index"), type = "string")
  fitin$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  fitin$setExpectedSize(rows = 1, cols = 2)

  # information criteria
  fitms[["incrits"]] <- fitic <- createJaspTable(gettext("Information criteria"))
  fitic$addColumnInfo(name = "index", title = "",               type = "string")
  fitic$addColumnInfo(name = "value", title = gettext("Value"), type = "number", format = "sf:4;dp:3")
  fitic$setExpectedSize(rows = 1, cols = 2)

  # other fit measures
  fitms[["others"]] <- fitot <- createJaspTable(gettext("Other fit measures"))
  fitot$addColumnInfo(name = "index", title = gettext("Metric"), type = "string")
  fitot$addColumnInfo(name = "value", title = gettext("Value"),  type = "number", format = "sf:4;dp:3")
  fitot$setExpectedSize(rows = 1, cols = 2)

  if (is.null(cfaResult)) return()

  # actually compute the fit measures
  fitMeasures <- lavaan::fitmeasures(cfaResult[["lav"]])

  # information criteria
  fitic[["index"]] <- c(
    gettext("Log-likelihood"),
    gettext("Number of free parameters"),
    gettext("Akaike (AIC)"),
    gettext("Bayesian (BIC)"),
    gettext("Sample-size adjusted Bayesian (SSABIC)")
  )
  fitic[["value"]] <- fitMeasures[c("logl", "npar", "aic", "bic", "bic2")]
  if(is.na(fitMeasures["logl"]) && is.na(fitMeasures["aic"]) && is.na(fitMeasures["bic"]))
    fitic$setError("Information criteria are only available with ML-type estimators")

  # Fit indices
  fitin[["index"]] <- c(
    gettext("Comparative Fit Index (CFI)"),
    gettext("Tucker-Lewis Index (TLI)"),
    gettext("Bentler-Bonett Non-normed Fit Index (NNFI)"),
    gettext("Bentler-Bonett Normed Fit Index (NFI)"),
    gettext("Parsimony Normed Fit Index (PNFI)"),
    gettext("Bollen's Relative Fit Index (RFI)"),
    gettext("Bollen's Incremental Fit Index (IFI)"),
    gettext("Relative Noncentrality Index (RNI)")
  )

  # other fitmeasures
  fitot[["index"]] <- c(
    gettext("Root mean square error of approximation (RMSEA)"),
    gettextf("RMSEA 90%% CI lower bound"),
    gettextf("RMSEA 90%% CI upper bound"),
    gettext("RMSEA p-value"),
    gettext("Standardized root mean square residual (SRMR)"),
    gettextf("Hoelter's critical N (%s = .05)","\u03B1"),
    gettextf("Hoelter's critical N (%s = .01)","\u03B1"),
    gettext("Goodness of fit index (GFI)"),
    gettext("McDonald fit index (MFI)"),
    gettext("Expected cross validation index (ECVI)")
  )

  if (cfaResult[["orderedVariables"]] && !is.na(fitMeasures["chisq.scaled"])) {
    fitin[["value"]] <- fitMeasures[c("cfi.scaled", "tli.scaled", "nnfi.scaled", "nfi.scaled", "pnfi",
                                      "rfi.scaled", "ifi.scaled", "rni.scaled")]
    fitin$addFootnote(gettext("Except for the PNFI, the fit indices are scaled because of categorical variables in the data."))

    fitot[["value"]] <- fitMeasures[c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
                                      "rmsea.pvalue.scaled", "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
    fitot$addFootnote(gettext("The RMSEA results are scaled because of categorical variables in the data."))
  } else {
    fitin[["value"]] <- fitMeasures[c("cfi", "tli", "nnfi", "nfi", "pnfi", "rfi", "ifi", "rni")]
    fitot[["value"]] <- fitMeasures[c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue",
                                      "srmr", "cn_05", "cn_01", "gfi", "mfi", "ecvi")]
  }


  return()
}


.cfaTableParEst <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !is.null(jaspResults[["estimates"]])) return()

  jaspResults[["estimates"]] <- ests <- createJaspContainer(gettext("Parameter estimates"), position = 2)
  ests$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification",
                  "factorsUncorrelated", "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples",
                  "group", "invarianceTesting", "standardized", "ciLevel", "interceptsFixedToZero"))

  footnote <- NULL
  if (options[["seType"]] == "bootstrap" && nrow(cfaResult[["lav"]]@boot[["coef"]]) < options[["bootstrapSamples"]]) {
    footnote <- gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.",
                         nrow(cfaResult[["lav"]]@boot[["coef"]]))
  }

  #### TODO
  # - Also check testing

  if (options[["standardized"]] == "none" ||
      (options[["standardized"]] != "none" && options[["seType"]] == "bootstrap")) {
    pe <- lavaan::parameterEstimates(cfaResult[["lav"]], remove.eq = FALSE, remove.system.eq = TRUE,
                                     remove.ineq = FALSE, remove.def = FALSE, add.attributes = TRUE, boot.ci.type = "perc",
                                     level = options$ciLevel)
  } else {
    type <- switch(options[["standardized"]],
                   "latentVariables" = "std.lv",
                   "all" = "std.all",
                   "noExogenousCovariates" = "std.nox")

    pe <- lavaan::standardizedSolution(cfaResult[["lav"]], level = options[["ciLevel"]], type = type,
                                       remove.eq = FALSE, remove.ineq = FALSE, remove.def = FALSE)
    colnames(pe)[colnames(pe) == "est.std"] <- "est"
  }

  .cfaParEstToTablesHelper(pe, options, cfaResult, ests, footnote)

}

.cfaParEstToTablesHelper <- function(pei, options, cfaResult, jrobject, footnote) {

  pei <- as.data.frame(pei)
  facNames <- c(cfaResult[["spec"]]$latents)

  colSel <- c("lhs", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper")

  estTitle <- ifelse(options$standardized != "none", gettext("Std. estimate"), gettext("Estimate"))

  # First-order factor loadings ----
  # Set up table
  jrobject[["fl1"]] <- fl1 <- createJaspTable(title = gettext("Factor loadings"))
  if (!is.null(footnote)) fl1$addFootnote(footnote)

  if (options$group != "") {
    fl1$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  fl1$addColumnInfo(name = "lhs",   title = gettext("Factor"),    type = "string", combine = TRUE)
  fl1$addColumnInfo(name = "rhs",   title = gettext("Indicator"), type = "string")

  fl1$addColumnInfo(name = "est",    title  = estTitle,   type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "se",     title  = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "z",      title  = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
  fl1$addColumnInfo(name = "pvalue", title  = gettext("p"),          type = "number", format = "dp:3;p:.001")

  fl1$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  fl1$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                    overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  # add data
  fl1dat <- pei[pei$op == "=~" & !pei$rhs %in% facNames, colSel]
  fl1dat$lhs <- .translateFactorNames(fl1dat$lhs, options)
  if (options$group != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    fl1dat$group <- rep(groupLabs, each = nrow(fl1dat) / length(groupLabs))
  }

  fl1$setData(fl1dat)
  fl1$dependOn(optionsFromObject = jrobject)

  # Second-order factor loadings ----
  if (length(options$secondOrder) > 0) {
    # Set up table
    jrobject[["fl2"]] <- fl2 <- createJaspTable(title = gettext("Second-order factor loadings"))
    if (!is.null(footnote)) fl2$addFootnote(footnote)

    if (options$group != "") {
      fl2$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
    }
    fl2$addColumnInfo(name = "lhs",   title = gettext("Factor"),    type = "string", combine = TRUE)
    fl2$addColumnInfo(name = "rhs",   title = gettext("Indicator"), type = "string")

    fl2$addColumnInfo(name = "est",    title  = estTitle,   type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "se",     title  = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "z",      title  = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
    fl2$addColumnInfo(name = "pvalue", title  = gettext("p"),          type = "number", format = "dp:3;p:.001")

    fl2$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                      overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    fl2$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                      overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    # add data
    fl2dat <- pei[pei$op == "=~" & pei$rhs %in% facNames, colSel]

    fl2dat$rhs   <- .translateFactorNames(fl2dat$rhs, options)

    if (options$group != "") {
      fl2dat$group <- rep(groupLabs, each = nrow(fl2dat) / length(groupLabs))
    }
    fl2$setData(fl2dat)
    fl2$dependOn(optionsFromObject = jrobject)
  }


  # Factor variances ----
  # Set up table
  jrobject[["fv"]] <- fv <- createJaspTable(gettext("Factor variances"))
  if (!is.null(footnote)) fv$addFootnote(footnote)

  if (options$group != "") {
    fv$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  fv$addColumnInfo(name = "lhs",    title = gettext("Factor"),     type = "string", combine = TRUE)
  fv$addColumnInfo(name = "est",    title = estTitle,   type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "se",     title = gettext("Std. Error"), type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "z",      title = gettext("z-value"),    type = "number", format  = "sf:4;dp:3")
  fv$addColumnInfo(name = "pvalue", title = gettext("p"),          type = "number", format  = "dp:3;p:.001")

  fv$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  fv$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  # Add data
  fvdat     <- pei[pei$op == "~~" & pei$lhs %in% c(facNames, "SecondOrder") & pei$lhs == pei$rhs,
                   colSel[!colSel %in% c('rhs')]]
  fvdat$lhs <- .translateFactorNames(fvdat$lhs, options)
  if (options$group != "") {
    fvdat$group <- rep(groupLabs, each = nrow(fvdat) / length(groupLabs))
  }
  fv$setData(fvdat)
  fv$dependOn(optionsFromObject = jrobject)

  # Factor covariances ----
  hasMultipleFactorsAtTopLevel <-
    length(options$secondOrder) > 1 || (length(options$secondOrder) == 0 & length(options$factors) > 1)

  if (!options$factorsUncorrelated & hasMultipleFactorsAtTopLevel) {
    jrobject[["fc"]] <- fc <- createJaspTable(gettext("Factor Covariances"))
    if (!is.null(footnote)) fc$addFootnote(footnote)

    if (options$group != "") {
      fc$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
    }
    fc$addColumnInfo(name = "lhs",    title = "",                    type = "string")
    fc$addColumnInfo(name = "op",     title = "",                    type = "string")
    fc$addColumnInfo(name = "rhs",    title = "",                    type = "string")
    fc$addColumnInfo(name = "est",    title = estTitle,   type = "number", format = "sf:4;dp:3")
    fc$addColumnInfo(name = "se",     title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
    fc$addColumnInfo(name = "z",      title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
    fc$addColumnInfo(name = "pvalue", title = gettext("p"),          type = "number", format = "dp:3;p:.001")

    fc$addColumnInfo(name = "ci.lower", title = "Lower", type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    fc$addColumnInfo(name = "ci.upper", title = "Upper", type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    fcdat <- pei[pei$op == "~~" & pei$lhs %in% facNames & pei$lhs != pei$rhs, colSel]
    fcdat$lhs <- .translateFactorNames(fcdat$lhs, options)
    fcdat$rhs <- .translateFactorNames(fcdat$rhs, options)
    fcdat$op  <- rep("\u2194", nrow(fcdat))
    if (options$group != "") {
      fcdat$group <- rep(groupLabs, each = nrow(fcdat) / length(groupLabs))
    }
    fc$setData(fcdat)
    fc$dependOn(optionsFromObject = jrobject)
  }

  # Residual variances ----
  # Set up table
  jrobject[["rv"]] <- rv <- createJaspTable(gettext("Residual variances"))
  if (!is.null(footnote)) rv$addFootnote(footnote)

  if (options$group != "") {
    rv$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  rv$addColumnInfo(name = "lhs",    title = gettext("Indicator"),  type = "string", combine = TRUE)
  rv$addColumnInfo(name = "est",    title = estTitle,   type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "se",     title = gettext("Std. Error"), type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "z",      title = gettext("z-value"),    type = "number", format  = "sf:4;dp:3")
  rv$addColumnInfo(name = "pvalue", title = gettext("p"),          type = "number", format  = "dp:3;p:.001")

  rv$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  rv$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  # add data
  rvdat <- pei[pei$op == "~~" & !pei$lhs %in% facNames & !pei$lhs == "SecondOrder" &
                 pei$lhs == pei$rhs, colSel[!colSel %in% c('rhs')]]
  rvdat$lhs <- rvdat$lhs
  if (options$group != "") {
    rvdat$group <- rep(groupLabs, each = nrow(rvdat) / length(groupLabs))
  }
  rv$setData(rvdat)
  rv$dependOn(optionsFromObject = jrobject)

  # Residual covariances ----
  if (length(options$residualsCovarying) > 0) {

    jrobject[["rc"]] <- rc <- createJaspTable(gettext("Residual covariances"))
    if (!is.null(footnote)) rc$addFootnote(footnote)

    if (options$group != "") {
      rc$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
    }
    rc$addColumnInfo(name = "lhs",    title = "",                    type = "string")
    rc$addColumnInfo(name = "op",     title = "",                    type = "string")
    rc$addColumnInfo(name = "rhs",    title = "",                    type = "string")
    rc$addColumnInfo(name = "est",    title = estTitle,   type = "number", format = "sf:4;dp:3")
    rc$addColumnInfo(name = "se",     title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
    rc$addColumnInfo(name = "z",      title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
    rc$addColumnInfo(name = "pvalue", title = gettext("p"),          type = "number", format = "dp:3;p:.001")

    rc$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    rc$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    # add data
    rcdat <- pei[pei$op == "~~" & !pei$lhs %in% facNames & pei$lhs != pei$rhs, colSel]
    rcdat$op  <- rep("\u2194", nrow(rcdat))
    if (options$group != "") {
      rcdat$group <- rep(groupLabs, each = nrow(rcdat) / length(groupLabs))
    }
    rc$setData(rcdat)
    rc$dependOn(optionsFromObject = jrobject)
  }

  # Intercepts ----
  if (options$meanStructure) {

    if (options$group != "") {

      jrobject[["Factor Intercepts"]] <- fi <- createJaspTable(title = gettext("Factor Intercepts"))
      if (!is.null(footnote)) fi$addFootnote(footnote)

      fi$addColumnInfo(name = "group",  title = gettext("Group"), type = "string", combine = TRUE)
      fi$addColumnInfo(name = "lhs",    title = gettext("Factor"),     type = "string", combine = TRUE)
      fi$addColumnInfo(name = "est",    title = estTitle,   type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "se",     title = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "z",      title = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
      fi$addColumnInfo(name = "pvalue", title = gettext("p"),          type = "number", format = "dp:3;p:.001")

      fi$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
      fi$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                       overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))


      # add data
      fidat <- pei[pei$op == "~1" & pei$lhs %in% facNames, colSel[!colSel %in% 'rhs']]
      fidat$lhs <- .translateFactorNames(fidat$lhs, options)
      fidat$group <- rep(groupLabs, each = nrow(fidat) / length(groupLabs))
      fi$setData(fidat)
      fi$dependOn(optionsFromObject = jrobject)
    }

    # Manifest variable intercepts
    jrobject[["Intercepts"]] <- vi <- createJaspTable(title = gettext("Intercepts"))
    if (!is.null(footnote)) vi$addFootnote(footnote)

    if (options$group != "") {
      vi$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
    }
    vi$addColumnInfo(name = "lhs",    title  = gettext("Indicator"),  type = "string", combine = TRUE)
    vi$addColumnInfo(name = "est",    title  = estTitle,   type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "se",     title  = gettext("Std. Error"), type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "z",      title  = gettext("z-value"),    type = "number", format = "sf:4;dp:3")
    vi$addColumnInfo(name = "pvalue", title  = gettext("p"),          type = "number", format = "dp:3;p:.001")

    vi$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
    vi$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number", format = "sf:4;dp:3",
                     overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

    # add data
    vidat <- pei[pei$op == "~1" & !pei$lhs == "SecondOrder" & !pei$lhs %in% facNames,
                 colSel[!colSel %in% c('rhs')]]
    vidat$lhs <- vidat$lhs
    if (options$group != "") {
      vidat$group <- rep(groupLabs, each = nrow(vidat) / length(groupLabs))
    }
    vi$setData(vidat)
    vi$dependOn(optionsFromObject = jrobject)
  }


  # Thresholds
  if ("|" %in% pei$op) {
    .cfaThresholdsTable(jrobject, footnote, options, pei, colSel, cfaResult)
  }

}

.cfaThresholdsTable <- function(jrobject, footnote, options, pei, colSel, cfaResult) {
  # Manifest variable intercepts
  jrobject[["Thresholds"]] <- th <- createJaspTable(title = gettext("Thresholds"))
  if (!is.null(footnote)) th$addFootnote(footnote)

  if (options$group != "") {
    th$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  th$addColumnInfo(name = "lhs",    title  = gettext("Indicator"),  type = "string", combine = TRUE)
  th$addColumnInfo(name = "rhs",    title  = gettext("Threshold"),  type = "string")
  th$addColumnInfo(name = "est",    title  = gettext("Estimate"),   type = "number")
  th$addColumnInfo(name = "se",     title  = gettext("Std. Error"), type = "number")
  th$addColumnInfo(name = "z",      title  = gettext("z-value"),    type = "number")
  th$addColumnInfo(name = "pvalue", title  = gettext("p"),          type = "number")

  th$addColumnInfo(name = "ci.lower", title = gettext("Lower"), type = "number",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))
  th$addColumnInfo(name = "ci.upper", title = gettext("Upper"), type = "number",
                   overtitle = gettextf("%s%% Confidence Interval", options$ciLevel * 100))

  # add data
  thdat <- pei[pei$op == "|", colSel[!colSel %in% 'rhs']]
  thdat$lhs <- thdat$lhs
  thdat$rhs <- pei$rhs[pei$op == "|"]
  if (options$group != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    thdat$group <- rep(groupLabs, each = nrow(thdat) / length(groupLabs))
  }
  th$setData(thdat)
  th$dependOn(optionsFromObject = jrobject)
}

.cfaTableModIndices <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$modificationIndices || !is.null(jaspResults[["modind"]])) return()

  mi <- try(lavaan::modindices(cfaResult[["lav"]]))
  jaspResults[["modind"]] <- mic <- createJaspContainer(gettext("Modification Indices"), position = 5)
  mic$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated",
                 "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples", "group", "invarianceTesting", "modificationIndices",
                 "modificationIndicesCutoff", "interceptsFixedToZero"))

  if (isTryError(mi)) {
    mic$setError(.extractErrorMessage(mi))
  } else if (options$group != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (i in 1:length(groupLabs)) {
      mic[[groupLabs[i]]] <- createJaspContainer(groupLabs[i])
      mic[[groupLabs[i]]]$dependOn(optionsFromObject = mic)
      .cfaMItoTablesHelper(mi[mi$group == i, ], options, mic[[groupLabs[i]]], cfaResult)
    }
  } else {
    .cfaMItoTablesHelper(mi, options, mic, cfaResult)
  }
}

.cfaMItoTablesHelper <- function(mii, options, jrobject, cfaResult) {

  # cross loadings (first order)
  foc <- mii[mii$op == "=~" & mii$lhs %in% cfaResult[["spec"]]$latents, c("lhs", "rhs", "mi", "epc")]
  foc <- foc[foc$mi > options$modificationIndicesCutoff, ]

  if (nrow(foc) > 0) {
    foc <- as.data.frame(foc)
    foc <- foc[order(foc$mi, decreasing = TRUE), ]
    jrobject[["Cross-Loadings"]] <- focro <- createJaspTable(gettext("Cross-loadings"))
    focro$dependOn(optionsFromObject = jrobject)

    focro$addColumnInfo(name = "lhs", title = "",                   type = "string")
    focro$addColumnInfo(name = "op",  title = "",                   type = "string")
    focro$addColumnInfo(name = "rhs", title = "",                   type = "string")
    focro$addColumnInfo(name = "mi",  title = gettext("Mod. Ind."), type = "number", format = "sf:4;dp:3")
    focro$addColumnInfo(name = "epc", title = gettext("EPC"),       type = "number", format = "sf:4;dp:3")

    focro[["lhs"]] <- .translateFactorNames(foc$lhs, options)
    focro[["op"]]  <- rep("\u2192", nrow(foc))
    focro[["rhs"]] <- foc$rhs
    focro[["mi"]]  <- foc$mi
    focro[["epc"]] <- foc$epc
  }

  # cross loadings (second order)
  soc <- matrix(NA, 0, 0)
  if (length(options$secondOrder) > 1) {
    soc <- mii[mii$op == "=~" & mii$lhs %in% options$soLatents & mii$rhs %in% cfaResult[["spec"]]$latents,
               c("lhs", "rhs", "mi", "epc")]
    soc <- soc[soc$mi > options$modificationIndicesCutoff, ]

    if (nrow(soc) > 0) {
      soc <- as.data.frame(soc)
      soc <- soc[order(soc$mi, decreasing = TRUE), ]
      jrobject[["Second-Order Cross-Loadings"]] <- socro <- createJaspTable(gettext("Second-order cross-loadings"))
      socro$dependOn(optionsFromObject = jrobject)

      socro$addColumnInfo(name = "lhs", title  = "",                   type = "string")
      socro$addColumnInfo(name = "op",  title  = "",                   type = "string")
      socro$addColumnInfo(name = "rhs", title  = "",                   type = "string")
      socro$addColumnInfo(name = "mi",  title  = gettext("Mod. Ind."), type = "number", format = "sf:4;dp:3")
      socro$addColumnInfo(name = "epc", title  = gettext("EPC"),       type = "number", format = "sf:4;dp:3")


      socro[["lhs"]] <- soc$lhs
      socro[["op"]]  <- rep("\u2192", nrow(soc))
      socro[["rhs"]] <- soc$rhs
      socro[["mi"]]  <- soc$mi
      socro[["epc"]] <- soc$epc
    }
  }


  # residual covariances
  rec <- mii[mii$op == "~~" & !mii$lhs %in% c(cfaResult[["spec"]]$latents, cfaResult[["spec"]]$soLatents),
             c("lhs", "rhs", "mi", "epc")]
  rec <- rec[rec$mi > options$modificationIndicesCutoff, ]

  if (nrow(rec) > 0) {
    jrobject[["Residual Covariances"]] <- residualCovTable <- createJaspTable(gettext("Residual covariances"))
    residualCovTable$dependOn(optionsFromObject = jrobject)

    residualCovTable$addColumnInfo(name = "lhs", title  = "",                   type = "string")
    residualCovTable$addColumnInfo(name = "op",  title  = "",                   type = "string")
    residualCovTable$addColumnInfo(name = "rhs", title  = "",                   type = "string")
    residualCovTable$addColumnInfo(name = "mi",  title  = gettext("Mod. Ind."), type = "number", format = "sf:4;dp:3")
    residualCovTable$addColumnInfo(name = "epc", title  = gettext("EPC"),       type = "number", format = "sf:4;dp:3")

    rec <- as.data.frame(rec)
    rec <- rec[order(rec$mi, decreasing = TRUE), ]
    residualCovTable[["lhs"]] <- rec$lhs
    residualCovTable[["op"]]  <- rep("\u2194", nrow(rec))
    residualCovTable[["rhs"]] <- rec$rhs
    residualCovTable[["mi"]]  <- rec$mi
    residualCovTable[["epc"]] <- rec$epc
  }

  # display empty table when no modindex is above the cutoff
  if (nrow(foc) == 0 && nrow(soc) == 0 && nrow(rec) == 0) {
    jrobject[["empties"]] <- emptyModIndicesTable <- createJaspTable(gettext("Modification indices"))
    emptyModIndicesTable$dependOn(optionsFromObject = jrobject)
    emptyModIndicesTable$addFootnote(gettext("No modification index was above the cutoff"))
  }
}

.cfaTableImpliedCov <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$impliedCovarianceMatrix || !is.null(jaspResults[["impcov"]])) return()

  fv <- lavaan::fitted.values(cfaResult[["lav"]])

  if (options$group != "") {
    jaspResults[["impcov"]] <- icc <- createJaspContainer(gettext("Implied covariance matrices"), position = 3)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (l in groupLabs) {
      ic <- fv[[l]]$cov
      ic[upper.tri(ic)] <- NA
      tab <- createJaspTable(l)
      for (i in 1:ncol(ic)) {
        nm <- colnames(ic)[i]
        tab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3")
      }
      tab$addRows(ic, rowNames = colnames(ic))
      icc[[l]] <- tab
    }
  } else {
    ic <- fv$cov
    ic[upper.tri(ic)] <- NA
    icc <- createJaspTable(gettext("Implied covariance matrix"), position = 3)
    for (i in 1:ncol(ic)) {
      nm <- colnames(ic)[i]
      icc$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3")
    }
    icc$addRows(ic, rowNames = colnames(ic))
    jaspResults[["impcov"]] <- icc
  }

  icc$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification",
                 "factorsUncorrelated", "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples",
                 "group", "invarianceTesting", "impliedCovarianceMatrix", "interceptsFixedToZero"))
}

.cfaTableResCov <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$residualCovarianceMatrix || !is.null(jaspResults[["resCovTable"]])) return()
  rv <- lavaan::residuals(cfaResult[["lav"]])

  if (options$group != "") {
    jaspResults[["resCovTable"]] <- rcc <- createJaspContainer(gettext("Residual covariance matrices"), position = 4)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (l in groupLabs) {
      rc <- rv[[l]]$cov
      rc[upper.tri(rc)] <- NA
      tab <- createJaspTable(l)
      for (i in 1:ncol(rc)) {
        nm <- colnames(rc)[i]
        tab$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
      }
      tab$addRows(rc, rowNames = colnames(rc))
      rcc[[l]] <- tab
    }
  } else {
    rc <- rv$cov
    rc[upper.tri(rc)] <- NA
    rcc <- createJaspTable(gettext("Residual covariance matrix"), position = 4)
    for (i in 1:ncol(rc)) {
      nm <- colnames(rc)[i]
      rcc$addColumnInfo(nm, title = nm, type = "number", format = "sf:4;dp:3;p:.001")
    }
    rcc$addRows(rc, rowNames = colnames(rc))
    jaspResults[["resCovTable"]] <- rcc
  }

  rcc$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated",
                 "packageMimiced", "estimator", "naAction", "seType", "bootstrapSamples", "group", "invarianceTesting",
                 "residualCovarianceMatrix", "interceptsFixedToZero"))
}

.cfaInitPlots <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !(options$pathPlot || options$misfitPlot) || !is.null(jaspResults[["plots"]])) return()

  jaspResults[["plots"]] <- createJaspContainer(gettext("Plots"), position = 6)
  jaspResults[["plots"]]$dependOn(c(
    "factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated", "packageMimiced",
    "estimator", "naAction", "seType", "bootstrapSamples", "group", "invarianceTesting", "interceptsFixedToZero"
  ))
}

.cfaPlotPath <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$pathPlot || !is.null(jaspResults[["plots"]][["pathplot"]])) return()

  jaspBase:::.suppressGrDevice(
    pathplot <- semPlot::semPaths(
      object         = .cfaLavToPlotObj(cfaResult[["lav"]], options),
      DoNotPlot      = TRUE,
      ask            = FALSE,
      layout         = "tree",
      rotation       = ifelse(options$pathPlotRotated, 2, 1),
      intercepts     = options$pathPlotMean,
      whatLabels     = if (!options$pathPlotParameter) "name" else if (options$pathPlotStandardized) "std" else "par",
      mar            = if (!options$pathPlotRotated) ifelse(rep(is.null(options$secondOrder), 4), c(12, 3, 12, 3), c(6, 3, 6, 3)) else ifelse(rep(is.null(options$secondOrder), 4), c(3, 6, 3, 6), c(3, 3, 3, 3)),
      edge.color     = "black",
      color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
      border.width   = 1.5,
      edge.label.cex = options$pathPlotFontSize,
      lty            = 2,
      title          = FALSE,
      thresholds     = FALSE,
      residuals      = options$pathPlotVariance
    ))

  # set height depending on whether there is a second-order factor
  plotwidth  <- 640
  plotheight <- 320
  if (length(cfaResult[["spec"]][["soLatents"]]) > 0 && !options$pathPlotRotated) plotheight <- 500


  if (options$group != "") {
    jaspResults[["plots"]][["pathplot"]] <- createJaspContainer(gettext("Model plots"), position = 1)
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    for (i in 1:length(groupLabs)) {
      jaspResults[["plots"]][["pathplot"]][[groupLabs[i]]] <-
        createJaspPlot(pathplot[[i]], title = groupLabs[i], height = plotheight, width = plotwidth)
      jaspResults[["plots"]][["pathplot"]][[groupLabs[i]]]$dependOn(c("pathPlot", "pathPlotMean", "pathPlotParameter"))
    }
  } else {
    jaspResults[["plots"]][["pathplot"]] <- createJaspPlot(pathplot, title = gettext("Model plot"), height = plotheight,
                                                           width = plotwidth)
  }

  jaspResults[["plots"]][["pathplot"]]$dependOn(c("pathPlot", "pathPlotMean", "pathPlotParameter", "pathPlotStandardized", "pathPlotRotated", "pathPlotFontSize", "pathPlotVariance"))
}

.cfaLavToPlotObj <- function(lavResult, options) {
  # Create semplot model and unv the names of the manifest variables and backtranslate latent variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]

  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- decodeColNames(manifests)
  latents   <- semPlotMod@Vars$name[!semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[!semPlotMod@Vars$manifest] <- .translateFactorNames(latents, options)

  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest)) semPlotMod@Pars$lhs[lhsAreManifest] <- decodeColNames(semPlotMod@Pars$lhs[lhsAreManifest])
  lhsAreLatent   <- semPlotMod@Pars$lhs %in% latents
  if (any(lhsAreLatent))
    semPlotMod@Pars$lhs[lhsAreLatent] <- .translateFactorNames(semPlotMod@Pars$lhs[lhsAreLatent], options)


  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest)) semPlotMod@Pars$rhs[rhsAreManifest] <- decodeColNames(semPlotMod@Pars$rhs[rhsAreManifest])
  rhsAreLatent   <- semPlotMod@Pars$rhs %in% latents
  if (any(rhsAreLatent))
    semPlotMod@Pars$rhs[rhsAreLatent] <- .translateFactorNames(semPlotMod@Pars$rhs[rhsAreLatent], options)

  if (.hasSlot(semPlotMod, "Thresholds"))
    semPlotMod@Thresholds$lhs <- ifelse(nchar(semPlotMod@Thresholds$lhs) > 0, decodeColNames(semPlotMod@Thresholds$lhs), "")

  semPlotMod@Pars$label <- gsub("_", "", gsub("gamma", "\u03b3", gsub("lambda", "\u03bb", semPlotMod@Pars$label)))


  return(semPlotMod)
}

.cfaPlotMisfit <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options$misfitPlot || !is.null(jaspResults[["plots"]][["misfitplot"]])) return()
  rescor <- lavaan::residuals(cfaResult[["lav"]], type = "cor")
  wh <- 50 + 50 * length(cfaResult[["spec"]][["variables"]])

  if (options$group != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    jaspResults[["plots"]][["misfitplot"]] <- createJaspContainer(gettext("Misfit plots"), position = 2)
    for (i in 1:length(groupLabs)) {
      cc <- rescor[[i]]$cov
      cc[upper.tri(cc)] <- NA
      gg <- .resCorToMisFitPlot(cc)
      jaspResults[["plots"]][["misfitplot"]][[groupLabs[i]]] <-
        createJaspPlot(gg, title = groupLabs[i], width = wh, height = wh)
      jaspResults[["plots"]][["misfitplot"]][[groupLabs[i]]]$dependOn("misfitPlot")
    }
  } else {
    cc <- rescor$cov
    cc[upper.tri(cc)] <- NA
    gg <- .resCorToMisFitPlot(cc)
    jaspResults[["plots"]][["misfitplot"]] <- createJaspPlot(gg, title = gettext("Misfit plot"), width = wh, height = wh)
  }

  jaspResults[["plots"]][["misfitplot"]]$dependOn("misfitPlot")
}

.resCorToMisFitPlot <- function(rescor) {
  ggmisfit <- reshape2::melt(abs(t(rescor)))
  ggmisfit$labels <- substr(round(ggmisfit$value, 2), 2, 4)
  ggmisfit$labels[ggmisfit$labels == ""] <- "0"

  levels(ggmisfit$Var1) <- decodeColNames(levels(ggmisfit$Var1), strict = TRUE)
  levels(ggmisfit$Var2) <- decodeColNames(levels(ggmisfit$Var2), strict = TRUE)

  misfitplot <-
    ggplot2::ggplot(ggmisfit, ggplot2::aes(x = Var1, y = Var2, fill = value,
                                           label = labels)) +
    ggplot2::geom_tile(na.rm = TRUE) +
    ggplot2::geom_text(color = ifelse(ggmisfit$value > .5, "white", "black"),
                       na.rm = TRUE) +
    ggplot2::scale_y_discrete(limits = rev(levels(ggmisfit$Var1))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_continuous(low = "#FFFFFF", high = "#000000",
                                   na.value = "transparent",
                                   limits = c(0, 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 0)) +
    jaspGraphs::themeJaspRaw()

  return(misfitplot)
}

.cfaSyntax <- function(jaspResults, options, dataset, cfaResult) {
  if (is.null(cfaResult) || !options$lavaanSyntax || !is.null(jaspResults[["syntax"]])) return()

  mod <- .optionsToCFAMod(options, dataset, cfaResult, FALSE)$model

  jaspResults[["syntax"]] <- createJaspHtml(mod, class = "jasp-code", position = 7, title = gettext("Model syntax"))
  jaspResults[["syntax"]]$dependOn(optionsFromObject = jaspResults[["maincontainer"]][["cfatab"]])
  jaspResults[["syntax"]]$dependOn("lavaanSyntax")
}

.cfaTableAve <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options[["ave"]] || !is.null(jaspResults[["resAveTable"]])) return()

  aveTable <- createJaspTable(gettext("Average variance extracted"), position = 4.1)
  if (options[["group"]] != "") {
    aveTable$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  aveTable$addColumnInfo(name = "factor", title = gettext("Factor"), type = "string")
  aveTable$addColumnInfo(name = "ave", title = gettext("AVE"), type = "number")
  aveTable$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated",
                      "packageMimiced", "estimator", "naAction", "group", "invarianceTesting", "ave", "interceptsFixedToZero"))

  if (options$group != "") {
    ave_result <- semTools::AVE(cfaResult[["lav"]])
    groups <- cfaResult[["lav"]]@Data@group.label
    ave_result <- ave_result[, -1, drop = FALSE]
    aveTable[["group"]]   <- rep(groups, each = length(cfaResult[["spec"]][["latents"]]))
    aveTable[["factor"]]  <- rep(sapply(options[["factors"]], function(x) x[["title"]]), length(groups))
    aveTable[["ave"]]     <- c(t(ave_result))
  } else {
    ave_result <- semTools::AVE(cfaResult[["lav"]])
    aveTable[["factor"]] <- sapply(options[["factors"]], function(x) x[["title"]])
    aveTable[["ave"]] <- ave_result
  }
  jaspResults[["resAveTable"]] <- aveTable

}

.cfaTableHtmt <- function(jaspResults, options, cfaResult, dataset) {
  #### this has an ordering argument that still needs to be implemented once the categorical data stuff is done

  if (is.null(cfaResult) || !options[["htmt"]] || !is.null(jaspResults[["resHtmtTable"]])) return()

  htmtTable <- createJaspTable(gettext("Heterotrait-monotrait ratio"), position = 4.2)
  htmtTable$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated",
                       "packageMimiced", "estimator", "naAction", "group", "invarianceTesting", "htmt", "interceptsFixedToZero"))

  if (options[["group"]] != "") {
    htmtTable$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
    htmtTable$addColumnInfo(name = "faNames", title = "", type = "string")
  }

  facNames <- sapply(options[["factors"]], function(x) x[["title"]])
  for (fname in facNames) {
    htmtTable$addColumnInfo(name = fname, title = gettext(fname), type = "number")
  }

  if (options$group != "") {
    groups <- cfaResult[["lav"]]@Data@group.label
    # get the list of datasets per group
    dataList <- lavaan::inspect(cfaResult[["lav"]], what = "data")
    tmp_dat <- data.frame(group = rep(groups, each = length(facNames)),
                          faNames = rep(facNames, times = length(groups)))
    for (gg in groups) {
      ind <- which(gg == groups)
      dataGroup <- as.data.frame(dataList[[gg]])
      colnames(dataGroup) <- cfaResult[["lav"]]@Data@ov.names[[ind]]
      htmt_result <- semTools::htmt(model = cfaResult[["model_simple"]], data = dataGroup,
                                    missing = cfaResult[["lav"]]@Options[["missing"]])
      htmt_result[upper.tri(htmt_result)] <- NA
      tmp_dat[tmp_dat$group == gg, facNames] <- htmt_result
    }
    htmtTable$setData(tmp_dat)

  } else {
    if (options[["dataType"]] == "raw") {
      # get the dataset
      dataset <- as.data.frame(lavaan::inspect(cfaResult[["lav"]], what = "data"))
      colnames(dataset) <- cfaResult[["lav"]]@Data@ov.names[[1]]
      sampCov <- NULL
    } else {
      sampCov <- dataset
      colnames(sampCov) <- rownames(sampCov) <- cfaResult[["lav"]]@Data@ov.names[[1]]
      dataset <- NULL
    }

    if (is.null(cfaResult[["spec"]][["soIndics"]])) {
      htmt_result <- semTools::htmt(model = cfaResult[["model"]], data = dataset, sample.cov = sampCov,
                                    missing = cfaResult[["lav"]]@Options[["missing"]])
    } else { # the htmt does not allow a second order factor, so we take the model syntax without the seco
      htmt_result <- semTools::htmt(model = cfaResult[["model_simple"]], data = dataset, sample.cov = sampCov,
                                    missing = cfaResult[["lav"]]@Options[["missing"]])
    }

    htmt_result[upper.tri(htmt_result)] <- NA
    for (fname in facNames) {
      ii <- which(fname == facNames)
      htmtTable[[fname]] <- htmt_result[, ii]
    }
  }
  jaspResults[["resHtmtTable"]] <- htmtTable

  return()
}


.cfaTableReliability <- function(jaspResults, options, cfaResult) {
  if (is.null(cfaResult) || !options[["reliability"]] || !is.null(jaspResults[["resRelTable"]])) return()

  relTable <- createJaspTable(gettext("Reliability"), position = 4.3)
  if (options[["group"]] != "") {
    relTable$addColumnInfo(name = "group", title = gettext("Group"), type = "string", combine = TRUE)
  }
  relTable$addColumnInfo(name = "factor", title = "", type = "string")
  relTable$addColumnInfo(name = "rel", title = gettextf("Coefficient %s", "\u03C9"), type = "number")
  relTable$addColumnInfo(name = "alpha", title = gettextf("Coefficient %s", "\u03B1"), type = "number")
  relTable$dependOn(c("factors", "secondOrder", "residualsCovarying", "meanStructure", "modelIdentification", "factorsUncorrelated",
                      "packageMimiced", "estimator", "naAction", "group", "invarianceTesting", "reliability", "interceptsFixedToZero"))

  nfac <- length(cfaResult[["spec"]][["latents"]])

  rel_result_alpha <- semTools::compRelSEM(cfaResult[["lav"]], return.total = TRUE, tau.eq = TRUE)
  if (is.null(cfaResult[["spec"]][["soIndics"]])) {
    rel_result_omega <- semTools::compRelSEM(cfaResult[["lav"]], return.total = TRUE)
  } else {
    rel_result_omega <- semTools::compRelSEM(cfaResult[["lav"]], return.total = TRUE,
                                             higher = "SecondOrder")
    rel_result_alpha <- cbind(rel_result_alpha, NA)
  }

  if (options$group != "") {
    groups <- cfaResult[["lav"]]@Data@group.label
    rel_result_omega <- rel_result_omega[, -1, drop = FALSE]
    rel_result_alpha <- rel_result_alpha[, -1, drop = FALSE]
    relTable[["group"]]   <- rep(groups, each = ncol(rel_result_omega))
    relTable[["factor"]]  <- rep(c(sapply(options[["factors"]], function(x) x[["title"]]),
                                   colnames(rel_result_omega)[-(1:nfac)]), length(groups))
    relTable[["rel"]]     <- c(t(rel_result_omega))
    relTable[["alpha"]] <- c(t(rel_result_alpha))
  } else {
    relTable[["factor"]] <- c(sapply(options[["factors"]], function(x) x[["title"]]),
                              names(rel_result_omega)[-(1:nfac)])
    relTable[["rel"]] <- rel_result_omega
    relTable[["alpha"]] <- rel_result_alpha
  }
  jaspResults[["resRelTable"]] <- relTable

}

# delete once jaspSem is merged
lavBootstrap <- function(fit, samples = 1000, standard = FALSE, typeStd = NULL) {
  # Run bootstrap, track progress with progress bar
  # Notes: faulty runs are simply ignored
  # recommended: add a warning if not all boot samples are successful
  # fit <- lavBootstrap(fit, samples = 1000)
  # if (nrow(fit@boot$coef) < 1000)
  #  tab$addFootnote(gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.", nrow(fit@boot$coef)),
  #                  "<em>Note.</em>")


  coef_with_callback <- function(lav_object) {
    # Progress bar is ticked every time coef() is evaluated, which happens once on the main object:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L107
    # and then every time on a successful bootstrap:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L375
    # i.e., samples + 1 times
    progressbarTick()

    return(lavaan::coef(lav_object))
  }

  coef_with_callback_std <- function(lav_object, typeStd) {
    std <- lavaan::standardizedSolution(lav_object, type = typeStd)
    out <- std$est.std

    progressbarTick()

    return(out)
  }

  startProgressbar(samples + 1)

  if (!standard) {
    bootres <- lavaan::bootstrapLavaan(object = fit, R = samples, FUN = coef_with_callback)
  } else {
    bootres <- lavaan::bootstrapLavaan(object = fit, R = samples, FUN = coef_with_callback_std, typeStd = typeStd)
  }

  # Add the bootstrap samples to the fit object
  fit@boot       <- list(coef = bootres)
  fit@Options$se <- "bootstrap"

  # exclude error bootstrap runs
  err_id <- attr(fit@boot$coef, "error.idx")
  if (length(err_id) > 0L) {
    fit@boot$coef <- fit@boot$coef[-err_id, , drop = FALSE]
  }

  # we actually need the SEs from the bootstrap not the SEs from ML or something
  N <- nrow(fit@boot$coef)

  # we multiply the var by (n-1)/n because lavaan actually uses n for the variance instead of n-1
  if (!standard) {
    # for unstandardized
    fit@ParTable$se[fit@ParTable$free != 0] <- apply(fit@boot$coef, 2, sd) * sqrt((N-1)/N)
  } else {
    fit@ParTable$se <- apply(fit@boot$coef, 2, sd) * sqrt((N-1)/N)
    # the standardized solution gives all estimates not only the unconstrained, so we need to change
    # the free prameters in the partable and also change the estimate
    fit@ParTable$free <- seq_len(ncol(fit@boot$coef))
    std <- lavaan::standardizedSolution(fit, type = typeStd)
    fit@ParTable$est <- std$est.std
  }


  return(fit)
}


.cfaAddScoresToData <- function(jaspResults, options, cfaResult, dataset) {

  if (!is.null(jaspResults[["addedScoresContainer"]]) ||
      is.null(cfaResult) ||
      !options[["addScores"]] ||
      options[["dataType"]] == "varianceCovariance")
  {
    return()
  }


  container    <- createJaspContainer()
  container$dependOn(optionsFromObject = jaspResults[["maincontainer"]], options = c("addScores", "addedScoresPrefix",
                                                                                     "naAction", "factors",
                                                                                     "secondOrder"))

  scores <- lavaan::lavPredict(cfaResult[["lav"]])
  facNames <- cfaResult[["spec"]]$latents
  facNames <- .translateFactorNames(facNames, options)
  if (length(options$secondOrder) > 0)
    facNames <- c(facNames, gettext("Second-Order"))

  if (options$group != "") {
    groupLabs <- cfaResult[["lav"]]@Data@group.label
    colNamesR <- paste0(rep(groupLabs, each = ncol(scores[[1]])), "_", options[["addedScoresPrefix"]], "_", facNames)
  } else {
    colNamesR <- paste0(options[["addedScoresPrefix"]], "_", facNames)
    scores <- list(scores)
  }

  z <- 1
  for (ll in seq_len(length(scores))) {
    for (ii in seq_len(ncol(scores[[ll]]))) {

      colNameR <- colNamesR[z]
      scoresTmp <- scores[[ll]]
      if (jaspBase:::columnExists(colNameR) && !jaspBase:::columnIsMine(colNameR)) {
        .quitAnalysis(gettextf("Column name %s already exists in the dataset", colNameR))
      }

      container[[colNameR]] <- jaspBase::createJaspColumn(colNameR)
      if (options[["naAction"]] != "listwise") {
        container[[colNameR]]$setScale(scoresTmp[, ii])
      } else { # for listwise we need to identify the complete cases
        # so we need to temporarily load the raw data with the NAs
        dataTmp <- dataset
        scoresTmpTmp <- rep(NA, nrow(dataTmp))
        scoresTmpTmp[complete.cases(dataTmp)] <- scoresTmp[, ii]
        container[[colNameR]]$setScale(scoresTmpTmp)

      }
      z <- z + 1

    }
  }

  jaspResults[["addedScoresContainer"]] <- container

  # check if there are previous colNames that are not needed anymore and delete the cols
  oldNames <- jaspResults[["createdColumnNames"]][["object"]]
  newNames <- colNamesR[1:z]
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
