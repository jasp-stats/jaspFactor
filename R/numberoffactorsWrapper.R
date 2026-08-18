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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' numberOfFactors
#'
#' @param baseDecompositionOn, What type of correlation matrix to base the analysis on.
#' \itemize{
#'   \item \code{"correlationMatrix"}: The Pearson correlation matrix is used.
#'   \item \code{"polyTetrachoricCorrelationMatrix"}: The polychoric/tetrachoric correlation matrix is used. This is sometimes unstable when sample size is small and when some variables do not contain all response categories
#' }
#' @param dataType, Specifies whether the data is raw, meaning observations in rows and variables in columns, or whether the data is a variance-covariance matrix. For the latter, the sample size is required.
#' \itemize{
#'   \item \code{"varianceCovariance"}
#'   \item \code{"raw"}
#' }
#' @param eigenvaluesAbove, Threshold for the eigenvalue criterion: factors/components with an eigenvalue above this value are suggested for retention. The default of 1 corresponds to the Kaiser criterion.
#' @param naAction, Select how to handle missing values.
#' \itemize{
#'   \item \code{"pairwise"}: If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have an observation for all the variables to include the case in the analysis. This option is selected by default.
#'   \item \code{"listwise"}: If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis.
#' }
#' @param parallelAnalysisMethod, Whether the parallel analysis is based on principal component eigenvalues (PC) or factor eigenvalues (FA). PC-based parallel analysis is a common choice also when the goal is to determine the number of factors for a factor analysis.
#' \itemize{
#'   \item \code{"principalComponentBased"}
#'   \item \code{"factorBased"}
#' }
#' @param parallelAnalysisTable, Displays a table with the real data eigenvalues and the mean eigenvalues of the simulated data. Factors/components suggested for retention are marked with an asterisk.
#'    Defaults to \code{TRUE}.
#' @param screePlot, Displays a scree plot. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor/component. The horizontal line marks the eigenvalue threshold.
#'    Defaults to \code{TRUE}.
#' @param screePlotParallelAnalysisResults, Display the mean eigenvalues of the simulated data from the parallel analysis in the scree plot.
#'    Defaults to \code{TRUE}.
#' @param variables, In this box, the variables to perform the analysis on are selected
numberOfFactors <- function(
          data = NULL,
          version = "0.95",
          baseDecompositionOn = "correlationMatrix",
          dataType = "raw",
          eigenvaluesAbove = 1,
          naAction = "pairwise",
          parallelAnalysisMethod = "principalComponentBased",
          parallelAnalysisTable = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          sampleSize = 200,
          screePlot = TRUE,
          screePlotParallelAnalysisResults = TRUE,
          seed = 1,
          setSeed = FALSE,
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspFactor::numberOfFactors)
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

   optionsWithFormula <- c("variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFactor", "numberOfFactors", "NumberOfFactors.qml", options, version, TRUE))
}
