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
import QtQuick          2.8
import QtQuick.Layouts  1.3
import JASP.Controls    1.0
import JASP.Widgets		1.0

Form
{

	FactorsForm
	{
		id: factors
		name: "factors"
		initNumberFactors: 1
		allowAll: true
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
				suggestedColumns: []
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
			CheckBox { label: qsTr("Include mean structure")      ; name: "includemeanstructure"   ; id: meanstructure }
			CheckBox { label: qsTr("Assume factors uncorrelated") ; name: "uncorrelatedFactors"    }
			CheckBox { label: qsTr("Fix exogenous covariates")    ; name: "fixExogenousCovariates" ; checked: true ; visible: false }
			DropDown
			{
				label: qsTr("Factor Scaling")
				name: "identify"
				values: [
					{ label: qsTr("Factor variances"),	value: "factor"  },
					{ label: qsTr("Marker variable"),	value: "marker"  },
					{ label: qsTr("Effects coding"),	value: "effects" }
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
				AvailableVariablesList {name: "observedvars";	source: factors.name	}
				AssignedPairsVariablesList { name: "rescov" }
			}
		}
	}

	Section
	{
		title: qsTr("Additional Output")
		Group
		{
			CheckBox { label: qsTr("Additional fit measures")   ; name: "additionalfits"   }
			CheckBox { label: qsTr("R-Squared")                 ; name: "rsquared"         }
		}
		Group
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCov" }
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residCov"   }
			CheckBox {
				label: qsTr("Modification indices")
				name: "modIndices"
				DoubleField {
					label: qsTr("Cutoff")
					name: "miCutoff"
					min: 0
					defaultValue: 3.84
				}
			}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "showSyntax" }
		}
	}

	Section
	{
		title: qsTr("Multigroup CFA")
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "groupvar";
			showVariableTypeIcon: true;
			addEmptyValue: true;
		} // No model: it takes all variables per default
		DropDown
		{
			label: qsTr("Invariance testing")
			name: "invariance"
			values: [
				 { label: qsTr("Configural") , value: "configural"	},
				 { label: qsTr("Metric")     , value: "metric"		},
				 { label: qsTr("Scalar")     , value: "scalar"		},
				 { label: qsTr("Strict")     , value: "strict"		}
			]
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
				CheckBox { label: qsTr("Show means")      	; name: "pathPlotMean"	; enabled: meanstructure.checked }
				CheckBox { label: qsTr("Show variances")  	; name: "pathPlotVariance"; checked: true 	}
				CheckBox { label: qsTr("Rotate plot")		; name: "pathPlotRotated" 					}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")
		RadioButtonGroup
		{
			title: qsTr("Emulation")
			name: "mimic"
			RadioButton { label: qsTr("None")  ; value: "lavaan"  ; checked: true }
			RadioButton { label: qsTr("Mplus") ; value: "Mplus" }
			RadioButton { label: qsTr("EQS")   ; value: "EQS"   }
		}

		Group
		{
			title: qsTr("Error calculation")
			CIField { text: qsTr("CI width"); name: "ciWidth" }
			RadioButtonGroup
			{
				title: qsTr("Method")
				name: "se"
				RadioButton { label: qsTr("Standard")  ; value: "standard" ; checked: true }
				RadioButton { label: qsTr("Robust")    ; value: "robust" }
				RadioButton {
					label: qsTr("Bootstrap CI")
					value: "bootstrap"
					IntegerField {
						label: qsTr("Bootstrap samples")
						name: "bootstrapNumber"
						defaultValue: 1000
						min: 100
						max: 1000000
					}
				}
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Estimator")
			name: "estimator"
			RadioButton { label: qsTr("Auto") ; value: "default"; checked: true }
			RadioButton { label: qsTr("ML")   ; value: "ML"       }
			RadioButton { label: qsTr("GLS")  ; value: "GLS"      }
			RadioButton { label: qsTr("WLS")  ; value: "WLS"      }
			RadioButton { label: qsTr("ULS")  ; value: "ULS"      }
			RadioButton { label: qsTr("DWLS") ; value: "DWLS"     }
		}

		RadioButtonGroup
		{
			title: qsTr("Standardization")
			name: "std"
			RadioButton { label: qsTr("None")    ; value: "none"; checked: true }
			RadioButton { label: qsTr("Latents") ; value: "lv"  }
			RadioButton { label: qsTr("All")     ; value: "all" }
			RadioButton { label: qsTr("No X")    ; value: "nox" }
		}

		Group
		{
			title: qsTr("Options")
			debug: true
			CheckBox { label: qsTr("Fix manifest intercepts to zero")	; name: "fixManifestInterceptsToZero" }
			CheckBox { label: qsTr("Fix latent intercepts to zero")   	; name: "fixLatentInterceptsToZero"   ; checked: true }
			CheckBox { label: qsTr("Omit residual single indicator")  	; name: "omitResidualSingleIndicator" ; checked: true }
			CheckBox { label: qsTr("Residual variances")              	; name: "residualVariances"           ; checked: true }
			CheckBox { label: qsTr("Correlate exogenous latents")     	; name: "correlateExogenousLatents"   ; checked: true }
			CheckBox { label: qsTr("Add thresholds")                 	; name: "addThresholds"               ; checked: true }
			CheckBox { label: qsTr("Add scalings parameters")         	; name: "addScalingParameters"        ; checked: true }
			CheckBox { label: qsTr("Correlate dependent variables")   	; name: "correlateDependentVariables" ; checked: true }
		}
	}
}


