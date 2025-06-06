#
# Copyright (C) 2013-2024 University of Amsterdam
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

# This is a generated file. Don't change it

exploratoryFactorAnalysis <- function(
          data = NULL,
          version = "0.19.2",
          addScoresToData = FALSE,
          addScoresToDataPrefix = "FA",
          baseDecompositionOn = "correlationMatrix",
          bartlettTest = FALSE,
          dataType = "raw",
          eigenvaluesAbove = 1,
          factorCorrelations = FALSE,
          factorCountMethod = "parallelAnalysis",
          orderLoadingsBy = "size",
          factorStructure = FALSE,
          factoringMethod = "minimumResidual",
          fitIndices = FALSE,
          kaiserMeyerOlkinTest = FALSE,
          loadingsDisplayLimit = 0.4,
          manualNumberOfFactors = 1,
          mardiaTest = FALSE,
          naAction = "pairwise",
          obliqueSelector = "promax",
          orthogonalSelector = "none",
          parallelAnalysisMethod = "principalComponentBased",
          parallelAnalysisSeed = 1234,
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
          variables = list(types = list(), value = NULL)) {

   defaultArgCalls <- formals(jaspFactor::exploratoryFactorAnalysis)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("factoringMethod", "obliqueSelector", "orthogonalSelector", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFactor::exploratoryFactorAnalysis", data, options, version))
}
