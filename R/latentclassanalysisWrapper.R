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

# This is a generated file. Don't change it!

#' Latent Class Analysis
#'
#' @param indicators, Categorical indicator variables for the latent class model. At least two indicators are required.
#' @param itemResponseProbabilities, Show a table of item-response probabilities for each indicator and latent class.
#'    Defaults to \code{TRUE}.
#' @param itemResponseProbabilitiesPlot, Show a grouped bar chart of item-response probabilities, with one panel per latent class.
#'    Defaults to \code{FALSE}.
#' @param maxIterations, Maximum iterations for the EM algorithm.
#' @param missingValues, How to handle rows with missing values on any indicator.
#' \itemize{
#'   \item \code{"include"}
#'   \item \code{"listwise"}
#' }
#' @param nrep, Number of times the EM algorithm is run with different random starting values. The run with the highest log-likelihood is returned.
#' @param numberOfClasses, Models with 1, 2, …, K classes are fitted and compared.
#' @param rotatePlotLabels, Rotate the indicator names on the x-axis by 45 degrees to prevent overlapping.
#'    Defaults to \code{FALSE}.
#' @param saveClassProbabilities, Saves K columns of posterior class-membership probabilities (one per class) to the dataset.
#'    Defaults to \code{FALSE}.
#' @param saveClassProbabilitiesPrefix, Columns are named prefix_C1, prefix_C2, …
#' @param saveClassification, Saves the modal class assignment for each observation as a new nominal column.
#'    Defaults to \code{FALSE}.
#' @param saveClassificationColumn, Name of the column holding the most-likely class label for each observation.
#' @param saveForClasses, Which fitted model's results to save.
#' @param showLevelsLegend, Show a legend on the right indicating the color for each response category level.
#'    Defaults to \code{FALSE}.
latentClassAnalysis <- function(
          data = NULL,
          version = "1",
          indicators = list(types = list(), value = list()),
          itemResponseProbabilities = TRUE,
          itemResponseProbabilitiesPlot = FALSE,
          maxIterations = 1000,
          missingValues = "include",
          nrep = 1,
          numberOfClasses = 1,
          plotHeight = 320,
          plotWidth = 480,
          rotatePlotLabels = FALSE,
          saveClassProbabilities = FALSE,
          saveClassProbabilitiesPrefix = "classProb",
          saveClassification = FALSE,
          saveClassificationColumn = "classAssignment",
          saveForClasses = 1,
          seed = 1,
          setSeed = FALSE,
          showLevelsLegend = FALSE) {

   defaultArgCalls <- formals(jaspFactor::latentClassAnalysis)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("indicators")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFactor", "latentClassAnalysis", "LatentClassAnalysis.qml", options, version, TRUE))
}