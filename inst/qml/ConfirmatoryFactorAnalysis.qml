//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import JASP
import JASP.Controls

Form
{

	FactorsForm
	{
		id:					factors
		name:				"factors"
		initNumberFactors:	1
		allowedColumns:		["ordinal", "scale"]
		allowTypeChange:	true
		keepAvailableVariables: true
		info:				qsTr("Confirmatory factor analysis (CFA) models observed variables (indicators) as manifestations of underlying latent variables (factors). Scale and ordinal variables can be assigned to factors. There is a minimum of one factor, and each factor needs at least two indicators. Add a factor with the (+) button and remove it with the (-) button. A factor can be renamed by changing the name above its assignment box. If any ordinal variable is used, the estimator defaults to DWLS and the test statistic and fit measures are scaled and shifted (mean and variance adjusted, see the lavaan documentation). Other possible estimators are WLS and ULS. With ordinal variables it may make sense to use robust standard errors.")
	}
	Group 
	{
		// columns: 4
		title: qsTr("Data Type")
		RadioButtonGroup
		{
			id: dataType
			name: "dataType"
			columns: 2
			info: qsTr("Specifies whether the data is raw, meaning observations in rows and variables in columns, or whether the data is a variance-covariance matrix. For the latter, the sample size is required.")
			RadioButton { value: "raw"; label: qsTr("Raw"); checked: true; info: qsTr("Use raw data with observations as rows and variables in columns.") }
			RadioButton
			{
				value: "varianceCovariance"; label: qsTr("Variance-covariance matrix")
				info: qsTr("Use a variance-covariance matrix as input. Requires specifying the sample size.")
				IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 200; info: qsTr("The number of observations the variance-covariance matrix is based on.") }
			}
		}
	}

	Section
	{
		title: qsTr("Second-Order Factor")
		info: qsTr("The factors can in turn be indicators of a second-order factor. Any factors not assigned as indicators of the second-order factor are allowed to covary with each other and with the second-order factor, but not with the indicators of the second-order factor.")
		debug: false

		VariablesForm
		{
			id: secondorder
			AvailableVariablesList
			{
				name: "availableFactors"
				source: [{ name: "factors", use: "title" }]
				showVariableTypeIcon: false
				preferredHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
			}
			AssignedVariablesList
			{
				title: qsTr("Second-Order")
				name:  "secondOrder"
				info:  qsTr("Drag the factors that should be indicators of a second-order factor into this box.")
			}
			preferredHeight: jaspTheme.defaultVariablesFormHeight / 3
		}

		//		SEM.FactorsForm
		//		{
		//            id: secondorder
		//            name: "secondOrder"
		//            implicitHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
		//            allowAll: true
		//			availableVariablesList
		//			{
		//                name: "availableFactors"
		//				source: [{ name: "factors", use: "title" }]
		//                showVariableTypeIcon: false
		//                preferredHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
		//            }
		//            initNumberFactors: 1
		//        }
	}

	Section
	{
		title: qsTr("Model Options")
		info: qsTr("Options for the model specification: mean structure, factor correlations, identification and residual covariances.")
		columns: 1

		Group
		{
			title: qsTr("Model Options")
			CheckBox 
 			{ 
 				label: qsTr("Include mean structure")      ; 
 				name: "meanStructure"   ; 
 				id: meanstructure 
				info: qsTr("Display the means for the indicators and, in the case of multigroup CFA, the means of the latent variables. To identify the model, one of the following constraints needs to be chosen.")

 				RadioButtonGroup
 				{
 					// ChildrenOnSameRow: true
 					name: "interceptsFixedToZero"
					info: qsTr("Which intercepts are fixed to zero to identify the model with a mean structure.")
 					RadioButton { label: qsTr("Fix latent intercepts to zero") ;	value: "latent"; checked: true; info: qsTr("Fix the latent intercepts (factor means) to zero. The manifest intercepts are estimated freely.") }
 					RadioButton { label: qsTr("Fix manifest intercepts to zero"); value: "manifest"; info: qsTr("Fix the manifest intercepts to zero. The latent intercepts (factor means) are estimated freely.") }
					RadioButton { label: qsTr("Fix mean of manifest intercepts to zero"); value: "meanManifest"; info: qsTr("Fix the mean of the manifest intercepts to zero, which translates to effects coding. The latent intercepts (factor means) are estimated freely.") }

 				}
 			}
			CheckBox { label: qsTr("Assume factors uncorrelated") ; name: "factorsUncorrelated"; info: qsTr("Set the correlations between the different latent variables to 0.") }
			CheckBox { label: qsTr("Fix exogenous covariates")    ; name: "exogenousCovariatesFixed" ; checked: true ; visible: false }
			DropDown
			{
				label: qsTr("Model identification")
				name: "modelIdentification"
				info: qsTr("How the factors are given a scale. The default is marker variable.")
				values: [
					{ label: qsTr("Marker variable"),	value: "markerVariable", info: qsTr("The factor has the same scale as its first indicator, because the factor loading of that indicator is fixed to 1.") },
					{ label: qsTr("Factor variances"),	value: "factorVariance", info: qsTr("The factor has a fixed variance of 1.") },
					{ label: qsTr("Effects coding"),	value: "effectsCoding",  info: qsTr("The mean of the factor loadings of a factor is fixed to 1.") }
				]
			}
		}

		Group
		{
			title: qsTr("Residual Covariances")
			info: qsTr("Allow for covariance between indicators that is not explained by their respective factor, for example because the questions were phrased in a similar way. Drag two variables to the right-side assignment box to let them covary.")
			VariablesForm
			{
				id: rescov
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList 		 {	name: "observedVarsForResidualCov";	source: factors.name}
				AssignedPairsVariablesList {	name: "residualsCovarying"; info: qsTr("Pairs of indicators whose residuals are allowed to covary.") }
			}
		}
	}

	Section
	{
		title: qsTr("Multigroup CFA")
		info: qsTr("Fit the CFA model separately for each group of a grouping variable and optionally constrain parameters to be equal across groups.")
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "group";
			info: qsTr("Select a categorical variable here to create CFA models for each group.")
			showVariableTypeIcon: true
			addEmptyValue: true
			allowedColumns: ["nominal"]
		} // No model: it takes all variables per default
		DropDown
		{
			label: qsTr("Invariance testing")
			name: "invarianceTesting"
			info: qsTr("Select a level of constraining parameters over the different groups.")
			values: [
				 { label: qsTr("Configural") , value: "configural", info: qsTr("The different groups have the same CFA structure.") },
				 { label: qsTr("Metric")     , value: "metric",     info: qsTr("Same as configural, and the factor loadings are constrained to be equal across groups.") },
				 { label: qsTr("Scalar")     , value: "scalar",     info: qsTr("Same as metric, and the means of the indicators (intercepts) are constrained to be equal across groups.") },
				 { label: qsTr("Strict")     , value: "strict",     info: qsTr("Same as scalar, and the residual variances and residual covariances are constrained to be equal across groups.") },
				 { label: qsTr("Structural") , value: "structural", info: qsTr("Same as strict, and the latent means, variances, and covariances are constrained to be equal across groups.") }
			]
		}
	}


	Section
	{
		title: qsTr("Additional Output")
		info: qsTr("Additional tables with fit measures, assumption checks, validity and reliability measures, covariance matrices, modification indices and the lavaan syntax.")
		Group
		{
			CheckBox { label: qsTr("Additional fit measures")   ; name: "fitMeasures"; info: qsTr("Display the values of various fit measures: incremental fit indices (e.g., CFI, TLI), information criteria (log-likelihood, AIC, BIC, SSABIC) and other measures (e.g., RMSEA with 90% confidence interval, SRMR). Information criteria are only available with ML-type estimators.") }
			CheckBox { label: qsTr("Kaiser-Meyer-Olkin (KMO) test"); name: "kaiserMeyerOlkinTest"; info: qsTr("Determines how well the indicators are suited for factor analysis by computing the proportion of common variance between them. Produces a measure of sampling adequacy (MSA) for each indicator and overall; values closer to 1 are desired.") }
			CheckBox { label: qsTr("Bartlett's test of sphericity"); name: "bartlettTest"; info: qsTr("Tests whether the correlation matrix of the indicators is the identity matrix, meaning whether the indicators are related or not. A significant result means the correlation matrix is unlike the identity matrix.") }
			CheckBox { label: qsTr("R-Squared")                 ; name: "rSquared"; info: qsTr("Display the proportion of variance in each indicator that is explained by all predictors (the factors).") }
			CheckBox { name: "ave";						label: qsTr("Average variance extracted (AVE)"); info: qsTr("Display the average variance extracted: the amount of variance that is captured by a construct in relation to the amount of variance due to measurement error. Used to evaluate convergent validity.") }
			CheckBox { name: "htmt";					label: qsTr("Heterotrait-monotrait ratio (HTMT)"); info: qsTr("Display the heterotrait-monotrait ratio, a measure of the similarity between latent variables, to assess discriminant validity (Henseler et al., 2015). If the HTMT is clearly smaller than one, discriminant validity can be regarded as established.") }
			CheckBox { name: "reliability";		label: qsTr("Reliability"); info: qsTr("Display the reliability: the relative amount of test score variance that is explained by the factor(s). Contains coefficient omega (McDonald's) for each indicator group and in total and, if a second-order factor is specified, the omega of the second-order factor. Also provides coefficient alpha (Cronbach's). Note that the total test score variance in the denominator of the reliability equation is the observed test score variance, that is, the summed observed covariance matrix.") }
		}
		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovarianceMatrix"; info: qsTr("Display the covariance matrix implied by the model.") }
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovarianceMatrix"; info: qsTr("Display the covariances between the indicators that remain after taking the model into consideration. A perfect model shows all 0s here.") }
			CheckBox {
				label: qsTr("Modification indices")
				name: "modificationIndices"
				info: qsTr("Display modification indices (MI) above a minimum cutoff. A modification index shows how much the chi-square value of the overall fit would change if the parameter in question were freed up. The expected parameter change (EPC) shows the expected change of the parameter itself.")
				DoubleField {
					label: qsTr("Cutoff")
					name: "modificationIndicesCutoff"
					min: 0
					defaultValue: 3.84
					info: qsTr("Only modification indices above this value are displayed.")
				}
			}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "lavaanSyntax"; info: qsTr("Display the lavaan model syntax that would be needed to run the model in R.") }
		}
	}


	Section
	{
		title: qsTr("Plots")
		info: qsTr("Plots of the model fit and the estimated model structure.")
		Group
		{
			title: qsTr("Plots")
			CheckBox { label: qsTr("Misfit plot")     ; name: "misfitPlot"; info: qsTr("Visually show the residual correlations of the indicators, i.e., the correlations that remain unexplained by the model.") }
			CheckBox
			{
				label: qsTr("Model plot")
				name: "pathPlot"
				info: qsTr("Graphically show the estimated model structure.")
				CheckBox {
					label: qsTr("Show parameter estimates")
					name: "pathPlotParameter"
					info: qsTr("Display the parameter estimates on the model plot.")
					CheckBox { label: qsTr("Standardized")	; name: "pathPlotStandardized"; info: qsTr("Show standardized parameter estimates on the model plot.") }
					DoubleField { label: qsTr("Font size")	; name: "pathPlotFontSize"		; defaultValue: 0.9; max: 5.0; info: qsTr("Font size of the parameter estimates in the model plot.") }
				}
				CheckBox { label: qsTr("Show means")      	; name: "pathPlotMean"	; enabled: meanstructure.checked; info: qsTr("Show the means (intercepts) in the model plot. Only available when the mean structure is included.") }
				CheckBox { label: qsTr("Show variances")  	; name: "pathPlotVariance"; checked: true; info: qsTr("Show the (residual) variances of the indicators and factors in the model plot.") }
				CheckBox { label: qsTr("Rotate plot")		; name: "pathPlotRotated"; info: qsTr("Rotate the model plot by 90 degrees, so that the factors are on the left and the indicators on the right, instead of the factors at the top.") }
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")
		info: qsTr("Options for the estimation method, standard errors, missing data handling, and standardization of the parameter estimates.")

		Group {
			DropDown
			{
				name: "packageMimiced"
				label: qsTr("Mimic")
				info: qsTr("Emulate the results of different software.")
				values: [
					{ label: qsTr("Lavaan"), value: "lavaan" },
					{ label: qsTr("Mplus"), value: "mplus" },
					{ label: qsTr("EQS"), value: "eqs" }
				]
			}

			RowLayout
			{
				DropDown
				{
					name: "estimator"
					label: qsTr("Estimator	             ")
					id: estimator
					info: qsTr("Choose the estimation method. The default is ML if only scale variables are used and DWLS if ordinal variables are used. Some estimators set additional options for the test statistic and standard errors in the background. ML-based extensions: MLM (classic robust standard errors, Satorra-Bentler test statistic), MLMV (classic robust standard errors, scaled and shifted test statistic), MLMVS (classic robust standard errors, mean and variance adjusted Satterthwaite-style test statistic), MLF (first-order standard errors, standard test), MLR (Huber-White robust standard errors, Yuan-Bentler T2-star test statistic). Other extensions: WLSM and WLSMV imply DWLS with a scaled or mean and variance adjusted test and robust standard errors, ULSM and ULSMV imply ULS with a scaled or mean and variance adjusted test and robust standard errors. If a method is chosen under Standard errors, it overwrites the standard errors set by the estimator.")
					values: [
						{ label: qsTr("Default"), value: "default" },
						{ label: qsTr("ML"), value: "ml" },
						{ label: qsTr("GLS"), value: "gls" },
						{ label: qsTr("WLS"), value: "wls" },
						{ label: qsTr("ULS"), value: "uls" },
						{ label: qsTr("DWLS"), value: "dwls" },
						{ label: qsTr("DLS"), value: "dls" },
						{ label: qsTr("PML"), value: "pml" },
						{ label: qsTr("MLM"), value: "mlm" },
						{ label: qsTr("MLMV"), value: "mlmv" },
						{ label: qsTr("MLMVS"), value: "mlmvs" },
						{ label: qsTr("MLF"), value: "mlf" },
						{ label: qsTr("MLR"), value: "mlr" },
						{ label: qsTr("WLSM"), value: "wlsm" },
						{ label: qsTr("WLSMV"), value: "wlsmv" },
						{ label: qsTr("ULSM"), value: "ulsm" },
						{ label: qsTr("ULSMV"), value: "ulsmv" }
					]
				}

				HelpButton
				{
					toolTip: 					qsTr("Click for more information")
					helpPage:					"forQml/tooltipEstimators"
				}
			}
			

			
			DropDown
			{
				label: qsTr("Standard errors")
				name: "seType"
				id: errorCalc
				info: qsTr("How the standard errors of the parameters are computed. If bootstrap is chosen, the confidence intervals are percentile bootstrap confidence intervals.")
				values: [
					{ label: qsTr("Default"), 				value: "default"						},
					{ label: qsTr("Standard"),  			value: "standard" 					},
					{ label: qsTr("Robust"), 				value: "robust" 						},
					{ label: qsTr("Bootstrap"), 			value: "bootstrap"					}
				]
			}
			IntegerField
			{
				visible: errorCalc.value == "bootstrap"
				name: "bootstrapSamples"
				label: qsTr("     Bootstrap samples")
				defaultValue: 1000
				min: 100
				max: 1000000
				info: qsTr("The number of bootstrap samples used to compute the standard errors.")
			}
			CIField { text: qsTr("     CI level"); name: "ciLevel"; info: qsTr("The confidence level of the confidence intervals of the parameter estimates.") }
			
			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				info: qsTr("How missing values are handled. With ordinal variables only listwise deletion and pairwise are available.")
				values: factors.columnsTypes.includes("ordinal") ? [
					{ label: qsTr("Listwise deletion")	, value: "listwise",		info: qsTr("Cases with a missing value on any of the indicators are removed from the analysis.") },
					{ label: qsTr("Pairwise")			, value: "pairwise",		info: qsTr("The correlations or covariances are computed using all available pairs of observations.") }
				] : [
					{ label: qsTr("FIML")				, value: "fiml",			info: qsTr("Full-information maximum likelihood uses all available information of every case.") },
					{ label: qsTr("Listwise deletion")	, value: "listwise",		info: qsTr("Cases with a missing value on any of the indicators are removed from the analysis.") },
					{ label: qsTr("Pairwise")			, value: "pairwise",		info: qsTr("The correlations or covariances are computed using all available pairs of observations.") },
					{ label: qsTr("Two-stage")			, value: "twoStage",		info: qsTr("First, the saturated model is estimated with the EM algorithm; then the model is fitted to the estimated means and covariances.") },
					{ label: qsTr("Robust two-stage")	, value: "robustTwoStage",	info: qsTr("Same as two-stage, but with standard errors and test statistic that are robust against non-normality.") }
				]
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Standardization")
			name: "standardized"
			info: qsTr("Display standardized parameter estimates for different standardization schemes.")
			RadioButton { label: qsTr("None");										value: "none"; checked: true;	info: qsTr("Display the unstandardized parameter estimates.") }
			RadioButton { label: qsTr("Latents");									value: "latentVariables";		info: qsTr("Standardize based on the variances of the latent variables only.") }
			RadioButton { label: qsTr("All");											value: "all";					info: qsTr("Standardize based on the variances of both the observed and the latent variables.") }
			RadioButton { label: qsTr("No Exogenous Covariates");	value: "noExogenousCovariates";	info: qsTr("Standardize based on the variances of the observed and the latent variables, excluding exogenous covariates.") }
		}

		CheckBox
		{
			id: addScores
			name: "addFactorScoresToData"
			label: qsTr("Add factor scores to data")
			info: qsTr("Adds the estimated factor scores as new columns to the data set")
			enabled: factors.countVariables > 1 && dataType.value == "raw"

			TextField {
				name: "addFactorScoresToDataPrefix"
				label: qsTr("Prefix")
				defaultValue: qsTr("FS")
				fieldWidth: 80
				enabled: addScores.checked
			}
		}
		Group
		{
			title: qsTr("Options")
			debug: true
			CheckBox { label: qsTr("Fix manifest intercepts to zero")	; 	name: "manifestInterceptsFixedToZero" }
			CheckBox { label: qsTr("Fix latent intercepts to zero")   	; name: "latentInterceptsFixedToZero";		checked: true }
			CheckBox { label: qsTr("Omit residual single indicator")  	; name: "residualSingleIndicatorOmitted";	checked: true }
			CheckBox { label: qsTr("Residual variances")              	; name: "residualVariances";				checked: true }
			CheckBox { label: qsTr("Correlate exogenous latents")     	; name: "exogenousLatentsCorrelated";		checked: true }
			CheckBox { label: qsTr("Add thresholds")                 	; 	name: "thresholds";						checked: true }
			CheckBox { label: qsTr("Add scalings parameters")         	; name: "scalingParamaters";				checked: true }
			CheckBox { label: qsTr("Correlate dependent variables")   	; name: "dependentVariablesCorrelated";		checked: true }
		}
	}
}


