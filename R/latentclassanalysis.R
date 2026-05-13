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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.lcaModelDeps <- c("indicators", "numberOfClasses", "setSeed", "seed", "nrep", "maxIterations", "missingValues")

latentClassAnalysisInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Linzer, D.A. & Lewis, J.B. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. Journal of Statistical Software, 42(10), 1-29.")

  ready          <- length(options[["indicators"]]) >= 2
  modelContainer <- .lcaModelContainer(jaspResults, options)

  .lcaComputeResults(modelContainer, dataset, options, ready)
  .lcaFitTable(modelContainer, options, ready)
  .lcaPerModelContainers(modelContainer, dataset, options, ready)
  .lcaSaveToData(jaspResults, modelContainer, dataset, options, ready)
}


.lcaModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]]))
    return(jaspResults[["modelContainer"]])

  container <- createJaspContainer()
  container$dependOn(.lcaModelDeps)
  jaspResults[["modelContainer"]] <- container
  return(container)
}


.lcaComputeResults <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["model"]])) return()

  lcaState <- createJaspState()
  lcaState$dependOn(.lcaModelDeps)
  modelContainer[["model"]] <- lcaState

  if (!ready) return()

  indicators <- options[["indicators"]]
  lcaData    <- as.data.frame(lapply(
    dataset[, indicators, drop = FALSE],
    function(x) as.integer(droplevels(as.factor(x)))
  ))

  f <- as.formula(paste0("cbind(", paste(indicators, collapse = ", "), ") ~ 1"))

  K    <- options[["numberOfClasses"]]
  fits <- lapply(seq_len(K), function(k) {
    .setSeedJASP(options)
    try(poLCA::poLCA(
      formula = f,
      data    = lcaData,
      nclass  = k,
      maxiter = options[["maxIterations"]],
      nrep    = options[["nrep"]],
      na.rm   = options[["missingValues"]] == "listwise",
      verbose = FALSE,
      graphs  = FALSE,
      calc.se = TRUE
    ))
  })

  lcaState$object <- fits
}


.lcaFitTable <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["fitTable"]])) return()

  t <- createJaspTable(gettext("Model Fit"))
  t$info     <- gettext("Fit statistics for each model. AIC and BIC penalize the log-likelihood for model complexity and can be used to compare models with different numbers of classes (lower is better). G² is the likelihood-ratio goodness-of-fit statistic; a non-significant p-value indicates adequate fit, though this test is sensitive to sparse cells and small samples.")
  t$position <- 1
  modelContainer[["fitTable"]] <- t

  t$addColumnInfo("classes", type = "integer", title = gettext("Classes"))
  t$addColumnInfo("aic",     type = "number",  title = "AIC")
  t$addColumnInfo("bic",     type = "number",  title = "BIC")
  t$addColumnInfo("llik",    type = "number",  title = gettext("Log-likelihood"), format = "dp:3")
  t$addColumnInfo("gsq",     type = "number",  title = gettext("G²"))
  t$addColumnInfo("df",      type = "integer", title = gettext("df"))
  t$addColumnInfo("nobs",    type = "integer", title = gettext("N"))

  if (!ready) return()

  fits <- modelContainer[["model"]]$object
  if (is.null(fits)) return()

  df <- do.call(rbind, lapply(seq_along(fits), function(k) {
    fit <- fits[[k]]
    if (jaspBase::isTryError(fit))
      return(data.frame(classes = k, aic = NA_real_, bic = NA_real_, llik = NA_real_,
                        gsq = NA_real_, df = NA_integer_, nobs = NA_integer_))
    data.frame(
      classes = k,
      aic     = fit$aic,
      bic     = fit$bic,
      llik    = fit$llik,
      gsq     = fit$Gsq,
      df      = fit$resid.df,
      nobs    = fit$Nobs
    )
  }))
  t$setData(df)
}


.lcaPerModelContainers <- function(modelContainer, dataset, options, ready) {
  if (!ready) return()

  K    <- options[["numberOfClasses"]]
  fits <- modelContainer[["model"]]$object

  for (k in seq_len(K)) {
    key   <- paste0("model_", k)
    title <- gettextf("%d Classes", k)

    if (is.null(modelContainer[[key]])) {
      mc <- createJaspContainer(title)
      mc$dependOn(c(.lcaModelDeps,
                    "itemResponseProbabilities",
                    "itemResponseProbabilitiesPlot",
                    "rotatePlotLabels"))
      mc$position      <- k + 1
      mc$initCollapsed <- K > 1
      modelContainer[[key]] <- mc
    } else {
      mc <- modelContainer[[key]]
    }

    fit <- if (!is.null(fits) && k <= length(fits)) fits[[k]] else NULL
    .lcaClassPrevalencesTable(mc, fit, k, ready)
    .lcaItemProbabilitiesContainer(mc, dataset, fit, k, options, ready)
    .lcaItemProbabilitiesPlot(mc, dataset, fit, k, options, ready)
  }
}


.lcaClassPrevalencesTable <- function(mc, fit, nclass, ready) {
  if (!is.null(mc[["prevalencesTable"]])) return()

  t <- createJaspTable(gettext("Class Prevalences"))
  t$info     <- gettext("Estimated sizes of the latent classes. Proportion is the model-estimated mixing weight (the probability that a randomly drawn observation belongs to each class), with its standard error. Modal proportion is the fraction of observations assigned to each class by their highest posterior probability; it may differ from the model proportion because modal assignment discretizes the continuous posterior.")
  t$position <- 1
  mc[["prevalencesTable"]] <- t

  t$addColumnInfo("class",           type = "string", title = gettext("Class"))
  t$addColumnInfo("proportion",      type = "number", title = gettext("Proportion"))
  t$addColumnInfo("se",              type = "number", title = gettext("SE"))
  t$addColumnInfo("modalProportion", type = "number", title = gettext("Modal proportion"))

  if (!ready || is.null(fit) || jaspBase::isTryError(fit)) return()

  t[["class"]]           <- gettextf("Class %d", seq_len(nclass))
  t[["proportion"]]      <- fit$P
  t[["se"]]              <- if (is.null(fit$P.se)) rep(NA_real_, nclass) else fit$P.se
  t[["modalProportion"]] <- tabulate(fit$predclass, nbins = nclass) / fit$Nobs
}


.lcaItemProbabilitiesContainer <- function(mc, dataset, fit, nclass, options, ready) {
  if (!options[["itemResponseProbabilities"]] || !is.null(mc[["probsContainer"]])) return()

  outer <- createJaspContainer(gettext("Item-Response Probabilities"))
  outer$info     <- gettext("Estimated probability of each response category for each indicator, conditional on class membership. Each row is a latent class; each column is a response category of the indicator. Probabilities within a row sum to 1.")
  outer$dependOn("itemResponseProbabilities")
  outer$position <- 2
  mc[["probsContainer"]] <- outer

  if (!ready || is.null(fit) || jaspBase::isTryError(fit)) return()

  indicators <- options[["indicators"]]

  for (i in seq_along(indicators)) {
    v       <- indicators[i]
    lvls    <- levels(droplevels(as.factor(dataset[[v]])))
    probMat <- fit$probs[[v]]

    tab <- createJaspTable(title = v)
    tab$position <- i
    tab$addColumnInfo("class", type = "string", title = gettext("Class"))
    for (j in seq_along(lvls))
      tab$addColumnInfo(paste0("cat", j), type = "number", title = lvls[j], format = "dp:3")

    df <- data.frame(class = gettextf("Class %d", seq_len(nclass)), stringsAsFactors = FALSE)
    for (j in seq_along(lvls))
      df[[paste0("cat", j)]] <- probMat[, j]
    tab$setData(df)
    outer[[v]] <- tab
  }
}


.lcaItemProbabilitiesPlot <- function(mc, dataset, fit, nclass, options, ready) {
  if (!options[["itemResponseProbabilitiesPlot"]] || !is.null(mc[["itemProbsPlot"]]))
    return()

  nIndicators  <- length(options[["indicators"]])
  showLegend   <- isTRUE(options[["showLevelsLegend"]])
  nLevels      <- if (showLegend) length(unique(unlist(lapply(options[["indicators"]], function(v)
    levels(droplevels(as.factor(dataset[[v]]))))))) else 0
  legendHeight <- nLevels * 22 + 50
  width        <- max(400, 120 * nIndicators) + if (showLegend) 160 else 0
  height       <- max(220 * nclass + if (options[["rotatePlotLabels"]]) 60 else 0, legendHeight)

  p <- createJaspPlot(
    title  = gettext("Item-Response Probabilities"),
    width  = width,
    height = height
  )
  p$info <- gettext("Grouped bar chart of item-response probabilities. Each panel shows one latent class; bars within each panel show the probability of each response category for every indicator. Taller bars indicate response categories more characteristic of that class.")
  p$dependOn(c("itemResponseProbabilitiesPlot", "rotatePlotLabels", "showLevelsLegend"))
  p$position <- 3
  mc[["itemProbsPlot"]] <- p

  if (!ready || is.null(fit) || jaspBase::isTryError(fit)) return()

  plotObj <- try(.lcaBuildItemProbsPlot(fit, dataset, options, nclass))
  if (jaspBase::isTryError(plotObj)) {
    p$setError(.extractErrorMessage(plotObj))
    return()
  }
  p$plotObject <- plotObj
}


.lcaBuildItemProbsPlot <- function(fit, dataset, options, nclass) {
  indicators        <- options[["indicators"]]
  decodedIndicators <- indicators

  df <- do.call(rbind, lapply(seq_along(indicators), function(i) {
    v       <- indicators[i]
    lvls    <- levels(droplevels(as.factor(dataset[[v]])))
    probMat <- fit$probs[[v]]
    do.call(rbind, lapply(seq_len(nclass), function(k) {
      data.frame(
        indicator   = decodedIndicators[i],
        class       = gettextf("Class %d", k),
        category    = lvls,
        probability = probMat[k, ],
        stringsAsFactors = FALSE
      )
    }))
  }))

  df$indicator <- factor(df$indicator, levels = decodedIndicators)

  # order levels by first appearance across indicators (not alphabetically)
  allLevels    <- unique(unlist(lapply(indicators, function(v)
    levels(droplevels(as.factor(dataset[[v]]))))))
  df$category  <- factor(df$category, levels = allLevels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = indicator, y = probability, fill = category)) +
    ggplot2::geom_col(position = "dodge", color = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(~class, ncol = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    jaspGraphs::scale_JASPfill_discrete(name = gettext("Level")) +
    ggplot2::labs(x = "", y = gettext("Probability")) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["showLevelsLegend"]]) "right" else "none") +
    ggplot2::theme(legend.key.height = ggplot2::unit(0.35, "cm"),
                  legend.key.width  = ggplot2::unit(0.5,  "cm"))

  if (options[["rotatePlotLabels"]])
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

  p
}


.lcaSaveToData <- function(jaspResults, modelContainer, dataset, options, ready) {
  wantProbs <- options[["saveClassProbabilities"]]
  wantClass <- options[["saveClassification"]]
  if (!wantProbs && !wantClass) return()

  saveDeps <- c(.lcaModelDeps, "saveForClasses",
                "saveClassProbabilities",   "saveClassProbabilitiesPrefix",
                "saveClassification",       "saveClassificationColumn")

  if (!is.null(jaspResults[["savedColumnsContainer"]])) return()

  container <- createJaspContainer()
  container$dependOn(saveDeps)
  jaspResults[["savedColumnsContainer"]] <- container

  if (!ready) return()

  fits <- modelContainer[["model"]]$object
  k    <- options[["saveForClasses"]]
  if (is.null(fits) || k > length(fits)) return()

  fit <- fits[[k]]
  if (jaspBase::isTryError(fit)) return()

  indicators <- options[["indicators"]]
  n          <- nrow(dataset)
  complete   <- if (options[["missingValues"]] == "listwise")
    complete.cases(dataset[, indicators, drop = FALSE])
  else
    rep(TRUE, n)

  if (wantProbs) {
    prefix    <- options[["saveClassProbabilitiesPrefix"]]
    posterior <- fit$posterior
    for (j in seq_len(k)) {
      colName <- paste0(prefix, "_C", j)
      if (jaspBase:::columnExists(colName) && !jaspBase:::columnIsMine(colName))
        .quitAnalysis(gettextf("Column '%s' already exists in the dataset.", colName))
      col             <- jaspBase::createJaspColumn(colName)
      vals            <- rep(NA_real_, n)
      vals[complete]  <- posterior[, j]
      col$setScale(vals)
      container[[colName]] <- col
    }
  }

  if (wantClass) {
    colName <- options[["saveClassificationColumn"]]
    if (jaspBase:::columnExists(colName) && !jaspBase:::columnIsMine(colName))
      .quitAnalysis(gettextf("Column '%s' already exists in the dataset.", colName))
    col            <- jaspBase::createJaspColumn(colName)
    vals           <- rep(NA_character_, n)
    vals[complete] <- paste0("Class ", fit$predclass)
    col$setNominal(vals)
    container[[colName]] <- col
  }
}
