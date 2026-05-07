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

.lcaModelDeps <- c("indicators", "numberOfClasses", "seed", "nrep", "maxIterations", "missingValues")

latentClassAnalysisInternal <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Linzer, D.A. & Lewis, J.B. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. Journal of Statistical Software, 42(10), 1-29.")

  ready          <- length(options[["indicators"]]) >= 2
  modelContainer <- .lcaModelContainer(jaspResults, options)

  .lcaComputeResults(modelContainer, dataset, options, ready)
  .lcaFitTable(modelContainer, options, ready)
  .lcaClassPrevalencesTable(modelContainer, options, ready)
  .lcaItemProbabilitiesContainer(modelContainer, dataset, options, ready)
  .lcaItemProbabilitiesPlot(modelContainer, dataset, options, ready)
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
  lcaData <- as.data.frame(lapply(
    dataset[, indicators, drop = FALSE],
    function(x) as.integer(droplevels(as.factor(x)))
  ))

  f <- as.formula(paste0("cbind(", paste(indicators, collapse = ", "), ") ~ 1"))

  set.seed(options[["seed"]])
  fit <- try(poLCA::poLCA(
    formula = f,
    data    = lcaData,
    nclass  = options[["numberOfClasses"]],
    maxiter = options[["maxIterations"]],
    nrep    = options[["nrep"]],
    na.rm   = options[["missingValues"]] == "listwise",
    verbose = FALSE,
    graphs  = FALSE,
    calc.se = TRUE
  ))

  if (jaspBase::isTryError(fit)) {
    modelContainer$setError(gettextf("Model failed: %s", .extractErrorMessage(fit)))
    return()
  }

  lcaState$object <- fit
}


.lcaFitTable <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["fitTable"]])) return()

  t <- createJaspTable(gettext("Model Fit"))
  t$position <- 1
  modelContainer[["fitTable"]] <- t

  t$addColumnInfo("aic",  type = "number",  title = "AIC")
  t$addColumnInfo("bic",  type = "number",  title = "BIC")
  t$addColumnInfo("llik", type = "number",  title = gettext("Log-likelihood"), format = "dp:3")
  t$addColumnInfo("gsq",  type = "number",  title = gettext("G²"))
  t$addColumnInfo("df",   type = "integer", title = gettext("df"))
  t$addColumnInfo("nobs", type = "integer", title = gettext("N"))

  if (!ready) return()

  fit <- modelContainer[["model"]]$object
  if (is.null(fit) || jaspBase::isTryError(fit)) return()

  t[["aic"]]  <- fit$aic
  t[["bic"]]  <- fit$bic
  t[["llik"]] <- fit$llik
  t[["gsq"]]  <- fit$Gsq
  t[["df"]]   <- fit$resid.df
  t[["nobs"]] <- fit$Nobs
}


.lcaClassPrevalencesTable <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["prevalencesTable"]])) return()

  t <- createJaspTable(gettext("Class Prevalences"))
  t$position <- 2
  modelContainer[["prevalencesTable"]] <- t

  t$addColumnInfo("class",      type = "string", title = gettext("Class"))
  t$addColumnInfo("proportion", type = "number", title = gettext("Proportion"))
  t$addColumnInfo("se",         type = "number", title = gettext("SE"))

  if (!ready) return()

  fit <- modelContainer[["model"]]$object
  if (is.null(fit) || jaspBase::isTryError(fit)) return()

  nclass <- options[["numberOfClasses"]]
  t[["class"]]      <- gettextf("Class %d", seq_len(nclass))
  t[["proportion"]] <- fit$P
  t[["se"]]         <- fit$P.se
}


.lcaItemProbabilitiesContainer <- function(modelContainer, dataset, options, ready) {
  if (!options[["itemResponseProbabilities"]] || !is.null(modelContainer[["probsContainer"]])) return()

  outer <- createJaspContainer(gettext("Item-Response Probabilities"))
  outer$dependOn("itemResponseProbabilities")
  outer$position <- 3
  modelContainer[["probsContainer"]] <- outer

  if (!ready) return()

  fit <- modelContainer[["model"]]$object
  if (is.null(fit) || jaspBase::isTryError(fit)) return()

  indicators <- options[["indicators"]]
  nclass     <- options[["numberOfClasses"]]

  for (i in seq_along(indicators)) {
    v       <- indicators[i]
    lvls    <- levels(droplevels(as.factor(dataset[[v]])))
    probMat <- fit$probs[[v]]   # nclass x ncategories matrix

    t <- createJaspTable(title = jaspBase::decodeColNames(v))
    t$position <- i
    t$addColumnInfo("class", type = "string", title = gettext("Class"))
    for (j in seq_along(lvls))
      t$addColumnInfo(paste0("cat", j), type = "number", title = lvls[j], format = "dp:3")

    df <- data.frame(class = gettextf("Class %d", seq_len(nclass)), stringsAsFactors = FALSE)
    for (j in seq_along(lvls))
      df[[paste0("cat", j)]] <- probMat[, j]
    t$setData(df)
    outer[[v]] <- t
  }
}


.lcaItemProbabilitiesPlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["itemResponseProbabilitiesPlot"]] || !is.null(modelContainer[["itemProbsPlot"]]))
    return()

  nIndicators <- length(options[["indicators"]])
  nclass      <- options[["numberOfClasses"]]
  width       <- max(400, 120 * nIndicators * min(nclass, 3))
  height      <- 320

  p <- createJaspPlot(
    title  = gettext("Item-Response Probabilities"),
    width  = width,
    height = height
  )
  p$dependOn(c("itemResponseProbabilitiesPlot", "rotatePlotLabels"))
  p$position <- 4
  modelContainer[["itemProbsPlot"]] <- p

  if (!ready) return()

  fit <- modelContainer[["model"]]$object
  if (is.null(fit) || jaspBase::isTryError(fit)) return()

  plotObj <- try(.lcaBuildItemProbsPlot(fit, dataset, options))
  if (jaspBase::isTryError(plotObj)) {
    p$setError(.extractErrorMessage(plotObj))
    return()
  }
  p$plotObject <- plotObj
}


.lcaBuildItemProbsPlot <- function(fit, dataset, options) {
  indicators        <- options[["indicators"]]
  nclass            <- options[["numberOfClasses"]]
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
    ggplot2::facet_wrap(~class) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(x = "", y = gettext("Probability"), fill = gettext("Category")) +
    jaspGraphs::geom_rangeframe(sides = "bl") +
    jaspGraphs::themeJaspRaw()

  if (options[["rotatePlotLabels"]])
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

  p
}
