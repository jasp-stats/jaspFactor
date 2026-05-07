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

.lcaModelDeps <- c("indicators", "models", "seed", "nrep", "maxIterations", "missingValues")

latentClassAnalysisInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Linzer, D.A. & Lewis, J.B. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. Journal of Statistical Software, 42(10), 1-29.")

  ready          <- length(options[["indicators"]]) >= 2
  modelContainer <- .lcaModelContainer(jaspResults, options)

  .lcaComputeResults(modelContainer, dataset, options, ready)
  .lcaFitTable(modelContainer, options, ready)
  .lcaPerModelContainers(modelContainer, dataset, options, ready)
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

  models <- options[["models"]]
  fits   <- lapply(seq_along(models), function(i) {
    nclass <- models[[i]][["numberOfClasses"]]
    set.seed(options[["seed"]])
    try(poLCA::poLCA(
      formula = f,
      data    = lcaData,
      nclass  = nclass,
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

  fits   <- modelContainer[["model"]]$object
  models <- options[["models"]]
  if (is.null(fits)) return()

  df <- do.call(rbind, lapply(seq_along(fits), function(i) {
    fit    <- fits[[i]]
    nclass <- models[[i]][["numberOfClasses"]]
    if (jaspBase::isTryError(fit))
      return(data.frame(classes = nclass, aic = NA_real_, bic = NA_real_, llik = NA_real_,
                        gsq = NA_real_, df = NA_integer_, nobs = NA_integer_))
    data.frame(
      classes = nclass,
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
  fits   <- if (ready) modelContainer[["model"]]$object else NULL
  models <- options[["models"]]

  for (i in seq_along(models)) {
    key    <- paste0("model_", i)
    nclass <- models[[i]][["numberOfClasses"]]
    title  <- gettextf("Model %1$d: %2$d Classes", i, nclass)

    if (is.null(modelContainer[[key]])) {
      mc <- createJaspContainer(title)
      mc$dependOn(c(.lcaModelDeps,
                    "itemResponseProbabilities",
                    "itemResponseProbabilitiesPlot",
                    "rotatePlotLabels"))
      mc$position       <- i + 1
      mc$initCollapsed  <- length(models) > 1
      modelContainer[[key]] <- mc
    } else {
      mc <- modelContainer[[key]]
    }

    fit <- if (!is.null(fits) && i <= length(fits)) fits[[i]] else NULL
    .lcaClassPrevalencesTable(mc, fit, nclass, ready)
    .lcaItemProbabilitiesContainer(mc, dataset, fit, nclass, options, ready)
    .lcaItemProbabilitiesPlot(mc, dataset, fit, nclass, options, ready)
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

    tab <- createJaspTable(title = jaspBase::decodeColNames(v))
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

  nIndicators <- length(options[["indicators"]])
  width       <- max(400, 120 * nIndicators)
  height      <- 220 * nclass

  p <- createJaspPlot(
    title  = gettext("Item-Response Probabilities"),
    width  = width,
    height = height
  )
  p$info <- gettext("Grouped bar chart of item-response probabilities. Each panel shows one latent class; bars within each panel show the probability of each response category for every indicator. Taller bars indicate response categories more characteristic of that class.")
  p$dependOn(c("itemResponseProbabilitiesPlot", "rotatePlotLabels"))
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
  decodedIndicators <- jaspBase::decodeColNames(indicators)

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
  df$category  <- factor(df$category)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = indicator, y = probability, fill = category)) +
    ggplot2::geom_col(position = "dodge", color = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(~class, ncol = 1) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "", y = gettext("Probability"), fill = gettext("Category")) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw()

  if (options[["rotatePlotLabels"]])
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

  p
}
