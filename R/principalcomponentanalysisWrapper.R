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

#' principalComponentAnalysis
#'
#' @param addScoresToData, Adds the estimated component/factor scores as new columns to the data set. The scores are regression scores.
#'    Defaults to \code{FALSE}.
#' @param antiImageCorrelationMatrix, Contains the negative partial correlations between pairs of variables after accounting for the effects of all other variables in the dataset. High values in the anti-image correlation matrix indicate that a variable might be redundant or have too much shared variance with other variables.
#'    Defaults to \code{FALSE}.
#' @param bartlettTest, Determines if the data correlation matrix is the identity matrix, meaning, if the variables are related or not. A significant result means the correlation matrix is unlike the identity matrix.
#'    Defaults to \code{FALSE}.
#' @param baseDecompositionOn, What to base the decomposition of the data into components/factors on.
#' \itemize{
#'   \item \code{"covarianceMatrix"}: The covariance matrix is used to decompose the data into components/factors.
#'   \item \code{"correlationMatrix"}: The correlation matrix is used to decompose the data into components/factors.
#'   \item \code{"polyTetrachoricCorrelationMatrix"}: The polychoric/tetrachoric correlation matrix is used to decompose the data into components/factors. This is sometimes unstable when sample size is small and when some variables do not contain all response categories
#' }
#' @param componentCorrelations, When selecting this option, a table with the correlations between the components/factors will be displayed.
#'    Defaults to \code{FALSE}.
#' @param dataType, Specifies whether the data is raw, meaning observations in rows and variables in columns, or whether the data is a variance-covariance matrix. For the latter, the sample size is required.
#' \itemize{
#'   \item \code{"raw"}
#'   \item \code{"varianceCovariance"}
#' }
#' @param factorStructure, An item by factor structure matrix. This is just the loadings (pattern) matrix times the factor intercorrelation matrix.
#'    Defaults to \code{FALSE}.
#' @param factoringMethod, Which factoring method to use for the decomposition.
#' \itemize{
#'   \item \code{"minimumResidual"} (default) : Perform a minimum residual factor analysis (minres) using the first derivative.
#'   \item \code{"maximumLikelihood"}: Perform a maximum likelihood factor analysis (ml).
#'   \item \code{"principalAxis"}: Perform a principal factor solution (pa).
#'   \item \code{"ordinaryLeastSquares"}: Minimize the residual matrix using an OLS procedure, slower but uses the empirical first derivative.
#'   \item \code{"weightedLeastSquares"}: Perform a weighted least squares (WLS) solution.
#'   \item \code{"generalizedLeastSquares"}: Perform a generalized least squares (GLS) solution.
#'   \item \code{"minimumChiSquare"}: Minimize the sample size-weighted chi-square using pairwise correlations.
#'   \item \code{"minimumRank"}: Perform a minimum rank factor analysis (minrank).
#' }
#' @param fitIndices, This option displays the Root Mean Squared Error of Approximation (RMSEA) with 90% confidence interval, the Tucker Lewis Index (TLI), and the Bayesian Information Criterion (BIC) to test the fit of the model.
#'    Defaults to \code{FALSE}.
#' @param kaiserMeyerOlkinTest, Determines how well variables are suited for factor analysis by computing the proportion of common variance between variables. Produces a measure of sampling adequacy (MSA) as the proportion of common variance among variables is computed for all variables; values closer to 1 are desired.
#'    Defaults to \code{FALSE}.
#' @param loadingsDisplayLimit, Loadings below this value will not be displayed in the output table.
#' @param mardiaTest, Assesses the degree of the departure from multivariate normality of the included variables in terms of multivariate skewness and kurtosis. The Mardia's test will always include the listwise complete cases.
#'    Defaults to \code{FALSE}.
#' @param naAction, Select how to handle missing values.
#' \itemize{
#'   \item \code{"listwise"}: If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. 
#'   \item \code{"pairwise"}: If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have an observation for all the variables to include the case in the analysis. This option is selected by default.
#' }
#' @param orderLoadingsBy, Either order the loadings by their size from large to small, or by variables, meaning according to their occurence in the variables list.
#' \itemize{
#'   \item \code{"size"}
#'   \item \code{"variables"}
#' }
#' @param parallelAnalysisTable, If this option is selected, a table will be generated exhibiting a detailed output of the parallel analysis. Can be based on principal component eigenvalues (PC) or factor eigenvalues (FA). The seed is taken from the parallel analysis for determining the number of components/factors above.
#'    Defaults to \code{FALSE}.
#' @param pathDiagram, By selecting this option, a visual representation of the direction and strength of the relation between the variable and factor will be displayed.
#'    Defaults to \code{FALSE}.
#' @param residualMatrix, Displays a table containing the residual variances and correlations.
#'    Defaults to \code{FALSE}.
#' @param rotationMethod, Here, the rotation method to apply to the components can be specified. Rotation ensures a simpler understanding of the data structure.
#' \itemize{
#'   \item \code{"oblique"}: This method produces components that allow for correlation between the components. This method is selected by default. Several possibilities are available. The default is promax.
#'   \item \code{"orthogonal"}: This method produces components that are uncorrelated. For this method, there are several possibilities that can be selected.
#' }
#' @param screePlot, When selecting this option, a scree plot will be displayed. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. A scree plot can be used to decide how many factors should be selected.
#'    Defaults to \code{FALSE}.
#' @param screePlotParallelAnalysisResults, Display the results of the parallel analysis in the scree plot. The parallel analysis will be based on PC or FA as defined by the option for the parallel analysis table.
#'    Defaults to \code{TRUE}.
#' @param variables, In this box, the variables to perform the analysis on are selected
principalComponentAnalysis <- function(
          data = NULL,
          version = "0.95",
          addScoresToData = FALSE,
          addScoresToDataPrefix = "PC",
          antiImageCorrelationMatrix = FALSE,
          bartlettTest = FALSE,
          baseDecompositionOn = "correlationMatrix",
          componentCorrelations = FALSE,
          componentCountMethod = "parallelAnalysis",
          dataType = "raw",
          eigenvaluesAbove = 1,
          factorStructure = FALSE,
          factoringMethod = "minimumResidual",
          fitIndices = FALSE,
          kaiserMeyerOlkinTest = FALSE,
          loadingsDisplayLimit = 0.4,
          manualNumberOfComponents = 1,
          mardiaTest = FALSE,
          naAction = "pairwise",
          obliqueSelector = "promax",
          orderLoadingsBy = "size",
          orthogonalSelector = "none",
          parallelAnalysisMethod = "principalComponentBased",
          parallelAnalysisTable = FALSE,
          parallelAnalysisTableMethod = "principalComponentBased",
          pathDiagram = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          residualMatrix = FALSE,
          rotationMethod = "oblique",
          sampleSize = 200,
          screePlot = FALSE,
          screePlotParallelAnalysisResults = TRUE,
          seed = 1,
          setSeed = FALSE,
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspFactor::principalComponentAnalysis)
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

   optionsWithFormula <- c("factoringMethod", "obliqueSelector", "orthogonalSelector", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFactor", "principalComponentAnalysis", "PrincipalComponentAnalysis.qml", options, version, TRUE))
}