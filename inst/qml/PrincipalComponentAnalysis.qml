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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0


Form
{


	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			id: variables
			name: "variables"
			title: qsTr("Variables")
			suggestedColumns: ["scale"]
			allowedColumns: ["scale"]
		}
	}


	RadioButtonGroup
	{
		name: "componentCountMethod"
		title: qsTr("Number of Components based on")
		RadioButton
		{
			value: "parallelAnalysis"; label: qsTr("Parallel analysis"); checked: true

			RadioButtonGroup
			{
				name:  "parallelAnalysisMethod"
				title: ""

				RadioButton
				{
					value:		"principalComponentBased"
					label:		qsTr("Based on principal components")
					checked:	true
				}
				RadioButton
				{
					value: "factorBased"
					label: qsTr("Based on factors")
				}
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
				name:			"manualNumberOfComponents"
				label:			qsTr("Number of components")
				defaultValue:	1
				min:			1
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
				DropDown { name: "obliqueSelector";
					values: [ "promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster", "geominQ" ] }
			}
		}

		RadioButtonGroup
		{
			name: "analysisBasedOn"
			title: qsTr("Base decomposition on")
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

		Slider
		{
			name: "loadingsDisplayLimit"
			label: qsTr("Display loadings above")
			value: 0.4
		}

		Group
		{
			RadioButtonGroup
			{
				name: "componentLoadingsOrder"
				title: qsTr("Order component loadings by")
				RadioButton	{ name: "sortByComponentSize";	label: qsTr("Component size");	checked: true		}
				RadioButton	{ name: "sortByVariables";		label: qsTr("Variables")							}
			}

			Group
			{
				title: qsTr("Table")
				CheckBox { name: "componentCorrelations";	label: qsTr("Component correlations")		}
			}
			Group
			{
				title: qsTr("Plots")
				CheckBox {
					name: "pathDiagram"
					label: qsTr("Path diagram")
				}
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

		RadioButtonGroup
		{
			name: "naAction"
			title: qsTr("Missing Values")
			RadioButton { value: "pairwise";		label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "listwise";		label: qsTr("Exclude cases listwise")					}
		}

		CheckBox
		{
			debug: true
			id: addPC
			name: "addComponentScores"
			text: qsTr("Add PC scores to data")
			enabled: variables.count > 1

			ComputedColumnField {
				name: 		"componentsPrefix"
				text: 		"Prefix: "
				fieldWidth: 120
				visible:    addPC.checked
			}
		}
	}
}
