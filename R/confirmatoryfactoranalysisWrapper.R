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

#' Confirmatory Factor Analysis
#'
#' @param averageVarianceExtracted, Display the average variance extracted: the amount of variance that is captured by a construct in relation to the amount of variance due to measurement error. Used to evaluate convergent validity.
#'    Defaults to \code{FALSE}.
#' @param bartlettTest, Tests whether the correlation matrix of the indicators is the identity matrix, meaning whether the indicators are related or not. A significant result means the correlation matrix is unlike the identity matrix.
#'    Defaults to \code{FALSE}.
#' @param bootstrapSamples, The number of bootstrap samples used to compute the standard errors.
#' @param ciLevel, The confidence level of the confidence intervals of the parameter estimates.
#' @param dataType, Specifies whether the data is raw, meaning observations in rows and variables in columns, or whether the data is a variance-covariance matrix. For the latter, the sample size is required.
#' \itemize{
#'   \item \code{"raw"}: Use raw data with observations as rows and variables in columns.
#'   \item \code{"varianceCovariance"}: Use a variance-covariance matrix as input. Requires specifying the sample size.
#' }
#' @param estimator, Choose the estimation method. The default is ML if only scale variables are used and DWLS if ordinal variables are used. Some estimators set additional options for the test statistic and standard errors in the background. ML-based extensions: MLM (classic robust standard errors, Satorra-Bentler test statistic), MLMV (classic robust standard errors, scaled and shifted test statistic), MLMVS (classic robust standard errors, mean and variance adjusted Satterthwaite-style test statistic), MLF (first-order standard errors, standard test), MLR (Huber-White robust standard errors, Yuan-Bentler T2-star test statistic). Other extensions: WLSM and WLSMV imply DWLS with a scaled or mean and variance adjusted test and robust standard errors, ULSM and ULSMV imply ULS with a scaled or mean and variance adjusted test and robust standard errors. If a method is chosen under Standard errors, it overwrites the standard errors set by the estimator.
#' @param factorScoreSavedToData, Adds the estimated factor scores as new columns to the data set
#'    Defaults to \code{FALSE}.
#' @param factors, Confirmatory factor analysis (CFA) models observed variables (indicators) as manifestations of underlying latent variables (factors). Scale and ordinal variables can be assigned to factors. There is a minimum of one factor, and each factor needs at least two indicators. Add a factor with the (+) button and remove it with the (-) button. A factor can be renamed by changing the name above its assignment box. If any ordinal variable is used, the estimator defaults to DWLS and the test statistic and fit measures are scaled and shifted (mean and variance adjusted, see the lavaan documentation). Other possible estimators are WLS and ULS. With ordinal variables it may make sense to use robust standard errors.
#' @param factorsUncorrelated, Set the correlations between the different latent variables to 0.
#'    Defaults to \code{FALSE}.
#' @param fitMeasures, Display the values of various fit measures: incremental fit indices (e.g., CFI, TLI), information criteria (log-likelihood, AIC, BIC, SSABIC) and other measures (e.g., RMSEA with 90% confidence interval, SRMR). Information criteria are only available with ML-type estimators.
#'    Defaults to \code{FALSE}.
#' @param group, Select a categorical variable here to create CFA models for each group.
#' @param heterotraitMonotraitRatio, Display the heterotrait-monotrait ratio, a measure of the similarity between latent variables, to assess discriminant validity (Henseler et al., 2015). If the HTMT is clearly smaller than one, discriminant validity can be regarded as established.
#'    Defaults to \code{FALSE}.
#' @param impliedCovarianceMatrix, Display the covariance matrix implied by the model.
#'    Defaults to \code{FALSE}.
#' @param invarianceTesting, Select a level of constraining parameters over the different groups.
#' \itemize{
#'   \item \code{"configural"} (default) : The different groups have the same CFA structure.
#'   \item \code{"metric"}: Same as configural, and the factor loadings are constrained to be equal across groups.
#'   \item \code{"scalar"}: Same as metric, and the means of the indicators (intercepts) are constrained to be equal across groups.
#'   \item \code{"strict"}: Same as scalar, and the residual variances and residual covariances are constrained to be equal across groups.
#'   \item \code{"structural"}: Same as strict, and the latent means, variances, and covariances are constrained to be equal across groups.
#' }
#' @param kaiserMeyerOlkinTest, Determines how well the indicators are suited for factor analysis by computing the proportion of common variance between them. Produces a measure of sampling adequacy (MSA) for each indicator and overall; values closer to 1 are desired.
#'    Defaults to \code{FALSE}.
#' @param lavaanSyntax, Display the lavaan model syntax that would be needed to run the model in R.
#'    Defaults to \code{FALSE}.
#' @param meanStructure, Display the means for the indicators and, in the case of multigroup CFA, the means of the latent variables. To identify the model, one of the following constraints needs to be chosen.
#'    Defaults to \code{FALSE}.
#' @param meanStructureInterceptFixedToZero, Which intercepts are fixed to zero to identify the model with a mean structure.
#' \itemize{
#'   \item \code{"latent"}: Fix the latent intercepts (factor means) to zero. The manifest intercepts are estimated freely.
#'   \item \code{"manifest"}: Fix the manifest intercepts to zero. The latent intercepts (factor means) are estimated freely.
#'   \item \code{"meanManifest"}: Fix the mean of the manifest intercepts to zero, which translates to effects coding. The latent intercepts (factor means) are estimated freely.
#' }
#' @param misfitPlot, Visually show the residual correlations of the indicators, i.e., the correlations that remain unexplained by the model.
#'    Defaults to \code{FALSE}.
#' @param modelIdentification, How the factors are given a scale. The default is marker variable.
#' \itemize{
#'   \item \code{"markerVariable"} (default) : The factor has the same scale as its first indicator, because the factor loading of that indicator is fixed to 1.
#'   \item \code{"factorVariance"}: The factor has a fixed variance of 1.
#'   \item \code{"effectsCoding"}: The mean of the factor loadings of a factor is fixed to 1.
#' }
#' @param modificationIndices, Display modification indices (MI) above a minimum cutoff. A modification index shows how much the chi-square value of the overall fit would change if the parameter in question were freed up. The expected parameter change (EPC) shows the expected change of the parameter itself.
#'    Defaults to \code{FALSE}.
#' @param modificationIndicesCutoff, Only modification indices above this value are displayed.
#' @param naAction, How missing values are handled. With ordinal variables only listwise deletion and pairwise are available.
#' \itemize{
#'   \item \code{"fiml"} (default) : Full-information maximum likelihood uses all available information of every case.
#'   \item \code{"listwise"}: Cases with a missing value on any of the indicators are removed from the analysis.
#'   \item \code{"pairwise"}: The correlations or covariances are computed using all available pairs of observations.
#'   \item \code{"twoStage"}: First, the saturated model is estimated with the EM algorithm; then the model is fitted to the estimated means and covariances.
#'   \item \code{"robustTwoStage"}: Same as two-stage, but with standard errors and test statistic that are robust against non-normality.
#' }
#' @param packageMimiced, Emulate the results of different software.
#' @param pathPlot, Graphically show the estimated model structure.
#'    Defaults to \code{FALSE}.
#' @param pathPlotFontSize, Font size of the parameter estimates in the model plot.
#' @param pathPlotMean, Show the means (intercepts) in the model plot. Only available when the mean structure is included.
#'    Defaults to \code{FALSE}.
#' @param pathPlotParameter, Display the parameter estimates on the model plot.
#'    Defaults to \code{FALSE}.
#' @param pathPlotRotated, Rotate the model plot by 90 degrees, so that the factors are on the left and the indicators on the right, instead of the factors at the top.
#'    Defaults to \code{FALSE}.
#' @param pathPlotStandardized, Show standardized parameter estimates on the model plot.
#'    Defaults to \code{FALSE}.
#' @param pathPlotVariance, Show the (residual) variances of the indicators and factors in the model plot.
#'    Defaults to \code{TRUE}.
#' @param rSquared, Display the proportion of variance in each indicator that is explained by all predictors (the factors).
#'    Defaults to \code{FALSE}.
#' @param reliability, Display the reliability: the relative amount of test score variance that is explained by the factor(s). Contains coefficient omega (McDonald's) for each indicator group and in total and, if a second-order factor is specified, the omega of the second-order factor. Also provides coefficient alpha (Cronbach's). Note that the total test score variance in the denominator of the reliability equation is the observed test score variance, that is, the summed observed covariance matrix.
#'    Defaults to \code{FALSE}.
#' @param residualCovarianceMatrix, Display the covariances between the indicators that remain after taking the model into consideration. A perfect model shows all 0s here.
#'    Defaults to \code{FALSE}.
#' @param residualsCovarying, Pairs of indicators whose residuals are allowed to covary.
#' @param sampleSize, The number of observations the variance-covariance matrix is based on.
#' @param seType, How the standard errors of the parameters are computed. If bootstrap is chosen, the confidence intervals are percentile bootstrap confidence intervals.
#' @param secondOrder, Drag the factors that should be indicators of a second-order factor into this box.
#' @param standardized, Display standardized parameter estimates for different standardization schemes.
#' \itemize{
#'   \item \code{"none"}: Display the unstandardized parameter estimates.
#'   \item \code{"latentVariables"}: Standardize based on the variances of the latent variables only.
#'   \item \code{"all"}: Standardize based on the variances of both the observed and the latent variables.
#'   \item \code{"noExogenousCovariates"}: Standardize based on the variances of the observed and the latent variables, excluding exogenous covariates.
#' }
confirmatoryFactorAnalysis <- function(
          data = NULL,
          version = "1",
          averageVarianceExtracted = FALSE,
          bartlettTest = FALSE,
          bootstrapSamples = 1000,
          ciLevel = 0.95,
          dataType = "raw",
          dependentVariablesCorrelated = TRUE,
          estimator = "default",
          exogenousCovariatesFixed = TRUE,
          exogenousLatentsCorrelated = TRUE,
          factorScoreSavedToData = FALSE,
          factorScoreSavedToDataPrefix = "FS",
          factors = list(list(indicators = list(), name = "Factor1", title = "Factor 1")),
          factorsUncorrelated = FALSE,
          fitMeasures = FALSE,
          group = list(types = "unknown", value = ""),
          heterotraitMonotraitRatio = FALSE,
          impliedCovarianceMatrix = FALSE,
          invarianceTesting = "configural",
          kaiserMeyerOlkinTest = FALSE,
          latentInterceptsFixedToZero = TRUE,
          lavaanSyntax = FALSE,
          manifestInterceptsFixedToZero = FALSE,
          meanStructure = FALSE,
          meanStructureInterceptFixedToZero = "latent",
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
          scalingParameter = TRUE,
          seType = "default",
          secondOrder = list(types = list(), value = list()),
          standardized = "none",
          thresholds = TRUE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("estimator", "factors", "group", "invarianceTesting", "modelIdentification", "naAction", "packageMimiced", "residualsCovarying", "seType", "secondOrder")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspFactor", "confirmatoryFactorAnalysis", "ConfirmatoryFactorAnalysis.qml", options, version, TRUE))
}