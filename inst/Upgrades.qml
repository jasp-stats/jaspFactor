import QtQuick 		2.12
import JASP.Module 	1.0

Upgrades
{

	Upgrade
	{
		functionName:		"PrincipalComponentAnalysis"
		fromVersion:		"0.16"
		toVersion:			"0.16.1"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"pc"
		}
	}


	Upgrade
	{

		functionName:		"ExploratoryFactorAnalysis"
		fromVersion:		"0.16"
		toVersion:			"0.16.1"

		ChangeSetValue
		{
			condition:	function(options) { return options["parallelMethod"] === undefined }
			name:		"parallelMethod"
			jsonValue:	"fa"
		}
	}

	Upgrade
	{
		functionName: 		"PrincipalComponentAnalysis"
		newFunctionName:	"principalComponentAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename
		{
			from:	"incl_GoF"
			to:		"goodnessOfFit"
		}
		ChangeRename
		{
			from:	"incl_fitIndices"
			to:		"fitIndices"
		}
		ChangeRename
		{
			from:	"incl_loadings"
			to:		"loadings"
		}
		ChangeRename
		{
			from:	"plotHeightPathDiagram"
			to:		"pathDiagramPlotHeight"
		}
		ChangeRename
		{
			from:	"plotHeightScreePlot"
			to:		"screePlotPlotHeight"
		}
		ChangeRename
		{
			from:	"plotWidthPathDiagram"
			to:		"pathDiagramPlotWidth"
		}
		ChangeRename
		{
			from:	"plotWidthScreePlot"
			to:		"screePlotPlotWidth"
		}
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
			to:		"numberOfComponents"
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
			to:		"componentLoadingsOrder"
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
		functionName: 		"ExploratoryFactorAnalysis"
		newFunctionName:	"exploratoryFactorAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename
		{
			from:	"incl_GoF"
			to:		"goodnessOfFit"
		}
		ChangeRename
		{
			from:	"incl_loadings"
			to:		"loadings"
		}
		ChangeRename
		{
			from:	"plotHeightPathDiagram"
			to:		"pathDiagramPlotHeight"
		}
		ChangeRename
		{
			from:	"plotHeightScreePlot"
			to:		"screePlotPlotHeight"
		}
		ChangeRename
		{
			from:	"plotWidthPathDiagram"
			to:		"pathDiagramPlotWidth"
		}
		ChangeRename
		{
			from:	"plotWidthScreePlot"
			to:		"screePlotPlotWidth"
		}
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
			from:	"eigenValuesBox"
			to:		"eigenValuesAbove"
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
					case "cor":		return "correlationMatrix";
					case "cov":		return "covarianceMatrix";
					case "mixed":	return "polyTetrachoricMatrix";
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
			to:		"factorLoadingsOrder"
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
			from:	"missingValues"
			to:		"naAction"
		}
	}


	Upgrade
	{
		functionName: 		"ConfirmatoryFactorAnalysis"
		newFunctionName:	"confirmatoryFactorAnalysis"
		fromVersion:		"0.16.3"
		toVersion:			"0.16.4"

		ChangeRename	{ from: "misfitplot";				to: "misfitPlot"				}
		ChangeRename	{ from: "pathplot";					to: "pathPlot"					}
		ChangeRename	{ from: "plotpars";					to: "pathPlotParameter"			}
		ChangeRename	{ from: "plotstd";					to: "pathPlotStandardized"		}
		ChangeRename	{ from: "plotmeans";				to: "pathPlotMean"				}

		ChangeRename	{
			from:	"includemeanstructure"
			to:		"meanStructure"
		}
		ChangeRename	{
			from:	"uncorrelatedFactors"
			to:		"factorUncorrelated"
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
			to:		"covarResiduals"
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
			to:		"showLavaanSyntax"
		}
		ChangeRename	{
			from:	"groupvar"
			to:		"groupingVariable"
		}
		ChangeRename	{
			from:	"invariance"
			to:		"invarianceTesting"
		}
		ChangeRename	{
			from:	"mimic"
			to:		"package"
		}
		ChangeRename	{
			from:	"ciWidth"
			to:		"ciLevel"
		}
		ChangeRename	{
			from:	"se"
			to:		"standardErrorType"
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

	}
}
