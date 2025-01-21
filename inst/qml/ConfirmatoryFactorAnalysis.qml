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
	}
	Group 
	{
		// columns: 4
		title: qsTr("Data")
		RadioButtonGroup
		{
			name: "dataType"
			columns: 2
			RadioButton { value: "raw"; label: qsTr("Raw"); checked: true }
			RadioButton
			{
				value: "varianceCovariance"; label: qsTr("Variance-covariance matrix")
				IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 200 }
			}
		}
	}

	Section
	{
		title: qsTr("Second-Order Factor")
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
		columns: 1

		Group
		{
			title: qsTr("Model Options")
			CheckBox 
 			{ 
 				label: qsTr("Include mean structure")      ; 
 				name: "meanStructure"   ; 
 				id: meanstructure 

 				RadioButtonGroup
 				{
 					// ChildrenOnSameRow: true
 					name: "interceptsFixedToZero"
 					RadioButton { label: qsTr("Fix latent intercepts to zero") ;	value: "latent"; checked: true}
 					RadioButton { label: qsTr("Fix manifest intercepts to zero"); value: "manifest"}
					RadioButton { label: qsTr("Fix mean of manifest intercepts to zero"); value: "meanManifest"}

 				}
 			}
			CheckBox { label: qsTr("Assume factors uncorrelated") ; name: "factorsUncorrelated"    }
			CheckBox { label: qsTr("Fix exogenous covariates")    ; name: "exogenousCovariatesFixed" ; checked: true ; visible: false }
			DropDown
			{
				label: qsTr("Model identification")
				name: "modelIdentification"
				values: [
					{ label: qsTr("Marker variable"),	value: "markerVariable" },
					{ label: qsTr("Factor variances"),	value: "factorVariance" },
					{ label: qsTr("Effects coding"),	value: "effectsCoding"  }
				]
			}
		}

		Group
		{
			title: qsTr("Residual Covariances")
			VariablesForm
			{
				id: rescov
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList 		 {	name: "observedVarsForResidualCov";	source: factors.name}
				AssignedPairsVariablesList {	name: "residualsCovarying"}
			}
		}
	}

	Section
	{
		title: qsTr("Multigroup CFA")
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "group";
			showVariableTypeIcon: true;
			addEmptyValue: true;
		} // No model: it takes all variables per default
		DropDown
		{
			label: qsTr("Invariance testing")
			name: "invarianceTesting"
			values: [
				 { label: qsTr("Configural") , value: "configural"},
				 { label: qsTr("Metric")     , value: "metric"		},
				 { label: qsTr("Scalar")     , value: "scalar"		},
				 { label: qsTr("Strict")     , value: "strict"		}, 
				 { label: qsTr("Structural") , value: "structural"}, 
			]
		}
	}


	Section
	{
		title: qsTr("Additional Output")
		Group
		{
			CheckBox { label: qsTr("Additional fit measures")   ; name: "fitMeasures"   }
			CheckBox { label: qsTr("Kaiser-Meyer-Olkin (KMO) test"); name: "kaiserMeyerOlkinTest"}
			CheckBox { label: qsTr("Bartlett's test of sphericity"); name: "bartlettTest"}
			CheckBox { label: qsTr("R-Squared")                 ; name: "rSquared"         }
			CheckBox { name: "ave";						label: qsTr("Average variance extracted (AVE)")		}
			CheckBox { name: "htmt";					label: qsTr("Heterotrait-monotrait ratio (HTMT)")	}
			CheckBox { name: "reliability";		label: qsTr("Reliability")					}
		}
		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCovarianceMatrix"		}
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residualCovarianceMatrix"		}
			CheckBox {
				label: qsTr("Modification indices")
				name: "modificationIndices"
				DoubleField {
					label: qsTr("Cutoff")
					name: "modificationIndicesCutoff"
					min: 0
					defaultValue: 3.84
				}
			}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "lavaanSyntax" }
		}
	}


	Section
	{
		title: qsTr("Plots")
		Group
		{
			title: qsTr("Plots")
			CheckBox { label: qsTr("Misfit plot")     ; name: "misfitPlot" }
			CheckBox
			{
				label: qsTr("Model plot")
				name: "pathPlot"
				CheckBox {
					label: qsTr("Show parameter estimates")
					name: "pathPlotParameter"
					CheckBox { label: qsTr("Standardized")	; name: "pathPlotStandardized" }
					DoubleField { label: qsTr("Font size")	; name: "pathPlotFontSize"		; defaultValue: 0.9; max: 5.0 }
				}
				CheckBox { label: qsTr("Show means")      	; name: "pathPlotMean"	; enabled: meanstructure.checked	}
				CheckBox { label: qsTr("Show variances")  	; name: "pathPlotVariance"; checked: true					}
				CheckBox { label: qsTr("Rotate plot")		; name: "pathPlotRotated"									}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")

		Group {
			DropDown
			{
				name: "packageMimiced"
				label: qsTr("Mimic")
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
					label: qsTr("Estimator")
					id: estimator
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
			}
			CIField { text: qsTr("     CI level"); name: "ciLevel" }
			
			DropDown
			{
				name: "naAction"
				label: qsTr("Missing data handling")
				values:
				[
					{ label: qsTr("Listwise deletion")	, value: "listwise"			},
					{ label: qsTr("FIML")				, value: "fiml"				},
					{ label: qsTr("Pairwise")			, value: "pairwise"			},
					{ label: qsTr("Two-stage")			, value: "twoStage"			},
					{ label: qsTr("Robust two-stage")	, value: "twoStageRobust"	},
				]
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Standardization")
			name: "standardized"
			RadioButton { label: qsTr("None");										value: "none"; checked: true	}
			RadioButton { label: qsTr("Latents");									value: "latentVariables"		}
			RadioButton { label: qsTr("All");											value: "all"					}
			RadioButton { label: qsTr("No Exogenous Covariates");	value: "noExogenousCovariates"	}
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


