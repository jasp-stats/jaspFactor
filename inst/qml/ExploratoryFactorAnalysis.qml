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

	VariablesForm
	{
		// preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			id: variables
			name: "variables"
			title: qsTr("Variables")
			allowedColumns: ["scale"]
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
	}

	Group
	{
		RadioButtonGroup
		{
			name: "factorCountMethod"
			title: qsTr("Number of Factors based on")
			RadioButton
			{
				value:      "parallelAnalysis";
				label:      qsTr("Parallel analysis");
				checked:    true

				RadioButtonGroup
				{
					name:   "parallelAnalysisMethod"
					title:  ""

					RadioButton
					{
						value:		"principalComponentBased"
						label:		qsTr("Based on PC")
						checked:	true
					}
					RadioButton
					{
						value: "factorBased"
						label: qsTr("Based on FA")
					}
				}
				IntegerField
				{
					name: 			"parallelAnalysisSeed"
					label: 			"Seed"
					defaultValue: 	1234
					fieldWidth: 	60
					min: 			1
					max: 			1e6
				}
			}

			RadioButton
			{
				value: "eigenValues"; label: qsTr("Eigenvalues")
				DoubleField {
					name:			"eigenValuesAbove"
					label:			qsTr("Eigenvalues above")
					defaultValue:	1
					decimals:		1
				}
			}
			RadioButton
			{
				value: "manual"; label: qsTr("Manual")
				IntegerField {
					name:			"manualNumberOfFactors"
					label:			qsTr("Number of factors")
					defaultValue:	1
					min:			1
				}
			}
		}

		Group
		{
			title: qsTr("Factoring method")
			DropDown
			{
				name: "factoringMethod"
				indexDefaultValue: 0
				values:
				[
					{ label: qsTr("Minimum residual"),			value: "minimumResidual"		},
					{ label: qsTr("Maximum likelihood"),		value: "maximumLikelihood"		},
					{ label: qsTr("Principal axis factoring"),	value: "principalAxis"			},
					{ label: qsTr("Ordinary least squares"),	value: "ordinaryLeastSquares"	},
					{ label: qsTr("Weighted least squares"),	value: "weightedLeastSquares"	},
					{ label: qsTr("Generalized least squares"), value: "generalizedLeastSquares"},
					{ label: qsTr("Minimum chi-square"),		value: "minimumChiSquare"		},
					{ label: qsTr("Minimum rank"),				value: "minimumRank"			}
				]
			}
		}

	}



	Group
	{
		RadioButtonGroup
		{
			name: "rotationMethod"
			title: qsTr("Rotation")
			RadioButton
			{
				value	: "orthogonal"
				label	: qsTr("Orthogonal")
				DropDown
				{
					name: "orthogonalSelector"
					values: [
						{ label: qsTr("none")	, value: "none"			},
						{ label: "varimax"		, value: "varimax"		},
						{ label: "quartimax"	, value: "quartimax"	},
						{ label: "bentlerT"		, value: "bentlerT"		},
						{ label: "equamax"		, value: "equamax"		},
						{ label: "geominT"		, value: "geominT"		}
					]
				}
			}
			RadioButton
			{
				value	: "oblique"
				label	: qsTr("Oblique")
				checked	: true
				DropDown { name: "obliqueSelector"; values: [ "promax", "oblimin", "simplimax", "bentlerQ", "cluster", "geominQ" ] }
			}
		}

		RadioButtonGroup
		{
			name: "analysisBasedOn"
			title: qsTr("Base analysis on")
			RadioButton
			{
				value: "correlationMatrix"
				label: qsTr("Correlation matrix")
				checked: true
			}
			RadioButton
			{
				value: "covarianceMatrix"
				label: qsTr("Covariance matrix")
			}
			RadioButton
			{
				value: "polyTetrachoricCorrelationMatrix"
				label: qsTr("Polychoric/tetrachoric correlation matrix")
			}
		}
	}

	Section
	{
		title: qsTr("Output Options")
		Group
		{
			Slider {
				name: "loadingsDisplayLimit"
				label: qsTr("Display loadings above")
				value: 0.4
			}
			RadioButtonGroup
			{
				name: "factorLoadingsOrder"
				title: qsTr("Order factor loadings by")
				RadioButton	{ name: "sortByFactorSize";		label: qsTr("Factor size");		checked: true		}
				RadioButton	{ name: "sortByVariables";		label: qsTr("Variables")							}
			}
		}

		Group
		{
			Group
			{
				title: qsTr("Tables")
				CheckBox { name: "factorStructure";			label: qsTr("Structure matrix")				}
				CheckBox { name: "factorCorrelations";	label: qsTr("Factor correlations")		}
				CheckBox { name: "fitIndices";					label: qsTr("Additional fit indices")	}
				CheckBox { name: "residualMatrix";			label: qsTr("Residual matrix")				}
				CheckBox {
					name:	"parallelAnalysisTable";
					label:	qsTr("Parallel analysis")
					RadioButtonGroup
					{
						name:   "parallelAnalysisTableMethod"
						title:  ""

						RadioButton
						{
							value:      "principalComponentBased"
							label:      qsTr("Based on PC")
							checked:    true
						}
						RadioButton
						{
							value: "factorBased"
							label: qsTr("Based on FA")
						}
					}
				}
			}

			Group
			{
				title: qsTr("Plots")
				CheckBox { name: "pathDiagram";	label: qsTr("Path diagram")				}
				CheckBox {
					name:  "screePlot";
					label: qsTr("Scree plot")

					CheckBox {
						name:		"screePlotParallelAnalysisResults"
						label:		qsTr("Parallel analysis results")
						checked:	true
					}
				}
			}
		}


		Group
		{
			title: qsTr("Assumption checks")
			CheckBox { name: "kaiserMeyerOlkinTest";	label: qsTr("KMO test")					}
			CheckBox { name: "bartlettTest";			label: qsTr("Bartlett's test")	}
			CheckBox { name: "mardiaTest";				label: qsTr("Mardia's test")	  }
		}
		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			RadioButton { value: "pairwise";	label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "listwise";	label: qsTr("Exclude cases listwise")					}
		}

		CheckBox
		{
			id: addScores
			name: "addScores"
			label: qsTr("Add FA scores to data")
			enabled: variables.count > 1

			TextField {
				name: "addedScoresPrefix"
				label: qsTr("Prefix")
				defaultValue: "FA"
				fieldWidth: 80
				enabled: addScores.checked
			}
		}
	}
}
