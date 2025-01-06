import QtQuick
import JASP.Module

Upgrades
{

	// These changes were introduced in https://github.com/jasp-stats/jaspFactor/pull/93
	Upgrade
	{
		functionName:       "PrincipalComponentAnalysis"
		newFunctionName:	"principalComponentAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"pc"
		}
	}

	// These changes were introduced in https://github.com/jasp-stats/jaspFactor/pull/93
	Upgrade
	{
		functionName:       "ExploratoryFactorAnalysis"
		newFunctionName:	"exploratoryFactorAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"fa"
		}
	}

	// These changes were introduced in https://github.com/jasp-stats/jaspFactor/pull/104
	Upgrade
	{
		functionName:		"ConfirmatoryFactorAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename	{ from: "misfitplot";			to: "misfitPlot"			}
		ChangeRename	{ from: "pathplot";				to: "pathPlot"				}
		ChangeRename	{ from: "plotpars";				to: "pathPlotParameter"		}
		ChangeRename	{ from: "plotstd";				to: "pathPlotStandardized"	}
		ChangeRename	{ from: "plotmeans";			to: "pathPlotMean"			}
	}

	// Renaming for Syntax: https://github.com/jasp-stats/jaspFactor/pull/109
	Upgrade
	{
		functionName: 		"principalComponentAnalysis"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"


		ChangeRename
		{
			from:	"factorMethod"
			to:		"componentCountMethod"
		}
		ChangeRename
		{
			from:	"parallelMethod"
			to:		"parallelAnalysisMethod"
		}
		ChangeJS
		{
			name:		"parallelAnalysisMethod"
			jsFunction:	function(options)
			{
				switch(options["parallelAnalysisMethod"])
				{
				case "pc":		return "principalComponentBased";
				case "fa":		return "factorBased";
				}
			}
		}
		ChangeRename
		{
			from:	"eigenValuesBox"
			to:		"eigenValuesAbove"
		}
		ChangeRename
		{
			from:	"numberOfFactors"
			to:		"manualNumberOfComponents"
		}
		ChangeRename
		{
			from:	"basedOn"
			to:		"analysisBasedOn"
		}
		ChangeJS
		{
			name:		"analysisBasedOn"
			jsFunction:	function(options)
			{
				switch(options["analysisBasedOn"])
				{
				case "correlation":				return "correlationMatrix";
				case "covariance":				return "covarianceMatrix";
				case "mixedCorrelationMatrix":	return "polyTetrachoricCorrelationMatrix"
				}
			}
		}
		ChangeRename
		{
			from:	"highlightText"
			to:		"loadingsDisplayLimit"
		}
		ChangeRename
		{
			from:	"componentLoadingsSort"
			to:		"loadingsOrder"
		}
		ChangeRename
		{
			from:	"incl_correlations"
			to:		"componentCorrelations"
		}
		ChangeRename
		{
			from:	"incl_pathDiagram"
			to:		"pathDiagram"
		}
		ChangeRename
		{
			from:	"incl_screePlot"
			to:		"screePlot"
		}
		ChangeRename
		{
			from:	"screeDispParallel"
			to:		"screePlotParallelAnalysisResults"
		}
		ChangeRename
		{
			from:	"missingValues"
			to:		"naAction"
		}
		ChangeRename
		{
			from:	"addPC"
			to:		"addComponentScores"
		}
		ChangeRename
		{
			from:	"PCPrefix"
			to:		"componentsPrefix"
		}
	}


	Upgrade
	{
		functionName: 		"exploratoryFactorAnalysis"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename
		{
			from:	"factorMethod"
			to:		"factorCountMethod"
		}
		ChangeRename
		{
			from:	"parallelMethod"
			to:		"parallelAnalysisMethod"
		}
		ChangeJS
		{
			name:		"parallelAnalysisMethod"
			jsFunction:	function(options)
			{
				switch(options["parallelAnalysisMethod"])
				{
				case "pc":		return "principalComponentBased";
				case "fa":		return "factorBased";
				}
			}
		}
		ChangeRename
		{
			from:	"parallelSeed"
			to:		"parallelAnalysisSeed"
		}
		ChangeRename
		{
			from:	"eigenValuesBox"
			to:		"eigenValuesAbove"
		}
		ChangeRename
		{
			from:	"numberOfFactors"
			to:		"manualNumberOfFactors"
		}
		ChangeRename
		{
			from:	"fitmethod"
			to:		"factoringMethod"
		}
		ChangeRename
		{
			from:	"basedOn"
			to:		"analysisBasedOn"
		}
		ChangeJS
		{
			name:		"analysisBasedOn"
			jsFunction:	function(options)
			{
				switch(options["analysisBasedOn"])
				{
				case "correlation":		return "correlationMatrix";
				case "covariance":		return "covarianceMatrix";
				case "mixed":			return "polyTetrachoricCorrelationMatrix";
				}
			}
		}
		ChangeRename
		{
			from:	"highlightText"
			to:		"loadingsDisplayLimit"
		}
		ChangeRename
		{
			from:	"factorLoadingsSort"
			to:		"loadingsOrder"
		}
		ChangeRename
		{
			from:	"incl_structure"
			to:		"factorStructure"
		}
		ChangeRename
		{
			from:	"incl_correlations"
			to:		"factorCorrelations"
		}
		ChangeRename
		{
			from:	"incl_fitIndices"
			to:		"fitIndices"
		}
		ChangeRename
		{
			from:	"incl_pathDiagram"
			to:		"pathDiagram"
		}
		ChangeRename
		{
			from:	"incl_screePlot"
			to:		"screePlot"
		}
		ChangeRename
		{
			from:	"screeDispParallel"
			to:		"screePlotParallelAnalysisResults"
		}
		ChangeRename
		{
			from:	"kmotest"
			to:		"kaiserMeyerOlkinTest"
		}
		ChangeRename
		{
			from:	"bartest"
			to:		"bartlettTest"
		}
		ChangeRename
		{
			from:	"martest"
			to:		"mardiaTest"
		}
		ChangeRename
		{
			from:	"missingValues"
			to:		"naAction"
		}
		ChangeJS
		{
			name:		"factoringMethod"
			jsFunction:	function(options)
			{
				switch(options["factoringMethod"])
				{
				case "minres":	return "minimumResidual";
				case "ml":		return "maximumLikelihood";
				case "pa":		return "principalAxis";
				case "ols":		return "ordinaryLeastSquares";
				case "wls":		return "weightedLeastSquares";
				case "gls":		return "generalizedLeastSquares";
				case "minchi":	return "minimumChiSquare";
				case "minrank":	return "minimumRank";
				}
			}
		}
	}

	Upgrade
	{
		functionName:		"ConfirmatoryFactorAnalysis"
		newFunctionName:	"confirmatoryFactorAnalysis"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename	{
			from:	"includemeanstructure"
			to:		"meanStructure"
		}
		ChangeRename	{
			from:	"uncorrelatedFactors"
			to:		"factorsUncorrelated"
		}
		ChangeRename	{
			from:	"fixExogenousCovariates"
			to:		"exogenousCovariatesFixed"
		}
		ChangeRename	{
			from:	"identify"
			to:		"modelIdentification"
		}
		ChangeJS
		{
			name:		"modelIdentification"
			jsFunction:	function(options)
			{
				switch(options["modelIdentification"])
				{
				case "factor":		return "factorVariance";
				case "marker":		return "markerVariable";
				case "effects":		return "effectsCoding";
				}
			}
		}
		ChangeRename	{
			from:	"rescov"
			to:		"residualsCovarying"
		}
		ChangeRename	{
			from:	"additionalfits"
			to:		"fitMeasures"
		}
		ChangeRename	{
			from:	"rsquared"
			to:		"rSquared"
		}
		ChangeRename	{
			from:	"impliedCov"
			to:		"impliedCovarianceMatrix"
		}
		ChangeRename	{
			from:	"residCov"
			to:		"residualCovarianceMatrix"
		}
		ChangeRename	{
			from:	"modIndices"
			to:		"modificationIndices"
		}
		ChangeRename	{
			from:	"miCutoff"
			to:		"modificationIndicesCutoff"
		}
		ChangeRename	{
			from:	"showSyntax"
			to:		"lavaanSyntax"
		}
		ChangeRename	{
			from:	"groupvar"
			to:		"group"
		}
		ChangeRename	{
			from:	"invariance"
			to:		"invarianceTesting"
		}
		ChangeRename	{
			from:	"mimic"
			to:		"packageMimiced"
		}
		ChangeRename	{
			from:	"ciWidth"
			to:		"ciLevel"
		}
		ChangeRename	{
			from:	"se"
			to:		"seType"
		}
		ChangeRename	{
			from:	"bootstrapNumber"
			to:		"bootstrapSamples"
		}
		ChangeRename	{
			from:	"std"
			to:		"standardized"
		}
		ChangeRename	{
			from:	"fixManifestInterceptsToZero"
			to:		"manifestInterceptsFixedToZero"
		}
		ChangeRename	{
			from:	"fixLatentInterceptsToZero"
			to:		"latentInterceptsFixedToZero"
		}
		ChangeRename	{
			from:	"omitResidualSingleIndicator"
			to:		"residualSingleIndicatorOmitted"
		}
		ChangeRename	{
			from:	"correlateExogenousLatents"
			to:		"exogenousLatentsCorrelated"
		}
		ChangeRename	{
			from:	"addThresholds"
			to:		"thresholds"
		}
		ChangeRename	{
			from:	"addScalingParameters"
			to:		"scalingParamaters"
		}
		ChangeRename	{
			from:	"correlateDependentVariables"
			to:		"dependentVariablesCorrelated"
		}
		ChangeJS
		{
			name:		"estimator"
			jsFunction:	function(options)
			{
				switch(options["estimator"])
				{
				case "ML":		return "maximumLikelihood";
				case "GLS":		return "generalizedLeastSquares";
				case "WLS":		return "weightedLeastSquares";
				case "ULS":		return "unweightedLeastSquares";
				case "DWLS":	return "diagonallyWeightedLeastSquares";
				default:		return options["estimator"]
				}
			}
		}
		ChangeJS
		{
			name:		"standardized"
			jsFunction:	function(options)
			{
				switch(options["standardized"])
				{
				case "lv":		return "latentVariables";
				case "nox":		return "noExogenousCovariates";
				default:		return options["standardized"]
				}
			}
		}
	}
}

