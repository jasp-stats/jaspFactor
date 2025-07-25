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

#' confirmatoryFactorAnalysis
#'
#' @param addFactorScoresToData, Adds the estimated factor scores as new columns to the data set
#'    Defaults to \code{FALSE}.
confirmatoryFactorAnalysis <- function(
          data = NULL,
          version = "0.95",
          addFactorScoresToData = FALSE,
          addFactorScoresToDataPrefix = "FS",
          ave = FALSE,
          bartlettTest = FALSE,
          bootstrapSamples = 1000,
          ciLevel = 0.95,
          dataType = "raw",
          dependentVariablesCorrelated = TRUE,
          estimator = "default",
          exogenousCovariatesFixed = TRUE,
          exogenousLatentsCorrelated = TRUE,
          factors = list(list(indicators = list(), name = "Factor1", title = "Factor 1")),
          factorsUncorrelated = FALSE,
          fitMeasures = FALSE,
          group = list(types = "unknown", value = ""),
          htmt = FALSE,
          impliedCovarianceMatrix = FALSE,
          interceptsFixedToZero = "latent",
          invarianceTesting = "configural",
          kaiserMeyerOlkinTest = FALSE,
          latentInterceptsFixedToZero = TRUE,
          lavaanSyntax = FALSE,
          manifestInterceptsFixedToZero = FALSE,
          meanStructure = FALSE,
          misfitPlot = FALSE,
          modelIdentification = "markerVariable",
          modificationIndices = FALSE,
          modificationIndicesCutoff = 3.84,
          naAction = "fiml",
          packageMimiced = "lavaan",
          pathPlot = FALSE,
          pathPlotFontSize = 0.9,
          pathPlotMean = FALSE,
          pathPlotParameter = FALSE,
          pathPlotRotated = FALSE,
          pathPlotStandardized = FALSE,
          pathPlotVariance = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          rSquared = FALSE,
          reliability = FALSE,
          residualCovarianceMatrix = FALSE,
          residualSingleIndicatorOmitted = TRUE,
          residualVariances = TRUE,
          residualsCovarying = list(),
          sampleSize = 200,
          scalingParamaters = TRUE,
          seType = "default",
          secondOrder = list(types = list(), value = list()),
          standardized = "none",
          thresholds = TRUE) {

   defaultArgCalls <- formals(jaspFactor::confirmatoryFactorAnalysis)
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

   optionsWithFormula <- c("estimator", "factors", "group", "invarianceTesting", "modelIdentification", "naAction", "packageMimiced", "residualsCovarying", "seType", "secondOrder")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFactor", "confirmatoryFactorAnalysis", "ConfirmatoryFactorAnalysis.qml", options, version, TRUE))
}