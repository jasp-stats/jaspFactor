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

#' latentClassAnalysis
#'
#' @param indicators, Categorical indicator variables for the latent class model. At least two indicators are required.
#'    Defaults to \code{list(types = list(), value = list())}.
#' @param maxIterations, Maximum iterations for the EM algorithm.
#'    Defaults to \code{1000}.
#' @param missingValues, How to handle rows with missing values on any indicator.
#'    Defaults to \code{"listwise"}.
#' @param nrep, Number of times the EM algorithm is run with different random starting values.
#'    Defaults to \code{1}.
#' @param numberOfClasses, Number of latent classes to estimate.
#'    Defaults to \code{2}.
#' @param seed, Random seed for reproducibility.
#'    Defaults to \code{1}.
latentClassAnalysis <- function(
      data            = NULL,
      version         = "0.96",
      indicators      = list(types = list(), value = list()),
      maxIterations   = 1000,
      missingValues   = "listwise",
      nrep            = 1,
      numberOfClasses = 2,
      seed            = 1) {

   defaultArgCalls <- formals(jaspFactor::latentClassAnalysis)
   defaultArgs     <- lapply(defaultArgCalls, eval)
   options         <- as.list(match.call())[-1L]
   options         <- lapply(options, eval)
   defaults        <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]]    <- NULL
   options[["version"]] <- NULL

   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("indicators")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula"))
         options[[name]] <- jaspBase::jaspFormula(options[[name]], data)
   }

   return(jaspBase::runWrappedAnalysis("jaspFactor", "latentClassAnalysis", "LatentClassAnalysis.qml", options, version, TRUE))
}
