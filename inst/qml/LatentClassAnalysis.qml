//
// Copyright (C) 2013-2025 University of Amsterdam
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
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			id:             indicators
			name:           "indicators"
			title:          qsTr("Indicators")
			allowedColumns: ["nominal", "ordinal"]
			info:           qsTr("Categorical indicator variables for the latent class model. At least two indicators are required.")
		}
	}

	IntegerField
	{
		id:           numberOfClassesField
		name:         "numberOfClasses"
		label:        qsTr("Maximum number of classes")
		value:        indicators.count > 1 ? Math.floor(indicators.count / 2) : 1
		min:          1
		max:          indicators.count > 0 ? indicators.count : 9999
		info:         qsTr("Models with 1, 2, …, K classes are fitted and compared.")
	}

	Section
	{
		title: qsTr("Estimation Options")

		Group
		{
			IntegerField
			{
				name:         "nrep"
				label:        qsTr("Number of random starts")
				defaultValue: 1
				min:          1
				info:         qsTr("Number of times the EM algorithm is run with different random starting values. The run with the highest log-likelihood is returned.")
			}

			IntegerField
			{
				name:         "maxIterations"
				label:        qsTr("Maximum iterations")
				defaultValue: 1000
				min:          1
				info:         qsTr("Maximum iterations for the EM algorithm.")
			}

			SetSeed {}
		}


		RadioButtonGroup
		{
			name:  "missingValues"
			title: qsTr("Missing values")
			info:  qsTr("How to handle rows with missing values on any indicator.")

			RadioButton { value: "include";  label: qsTr("Include");          checked: true }
			RadioButton { value: "listwise"; label: qsTr("Listwise deletion") }
		}
	}

	Section
	{
		title: qsTr("Output Options")

		Group 
		{
			title: qsTr("Tables")
			CheckBox
			{
				name:    "itemResponseProbabilities"
				label:   qsTr("Item-response probabilities")
				checked: true
				info:    qsTr("Show a table of item-response probabilities for each indicator and latent class.")
			}
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox
			{
				id:    itemResponseProbabilitiesPlot
				name:  "itemResponseProbabilitiesPlot"
				label: qsTr("Item-response probabilities")
				info:  qsTr("Show a grouped bar chart of item-response probabilities, with one panel per latent class.")

				CheckBox
				{
					name:    "rotatePlotLabels"
					label:   qsTr("Rotate x-axis labels 45°")
					enabled: itemResponseProbabilitiesPlot.checked
					info:    qsTr("Rotate the indicator names on the x-axis by 45 degrees to prevent overlapping.")
				}

				CheckBox
				{
					name:    "showLevelsLegend"
					label:   qsTr("Show levels legend")
					enabled: itemResponseProbabilitiesPlot.checked
					info:    qsTr("Show a legend on the right indicating the color for each response category level.")
				}
			}
		}

	}

	Section
	{
		title: qsTr("Save to Data")

		Group 
		{
			IntegerField
			{
				name:         "saveForClasses"
				label:        qsTr("Number of classes")
				defaultValue: indicators.count > 1 ? Math.floor(indicators.count / 2) : 1
				min:          1
				max:          numberOfClassesField.value
				info:         qsTr("Which fitted model's results to save.")
			}

			CheckBox
			{
				id:    saveProbs
				name:  "saveClassProbabilities"
				label: qsTr("Class probabilities")
				info:  qsTr("Saves K columns of posterior class-membership probabilities (one per class) to the dataset.")

				TextField
				{
					name:         "saveClassProbabilitiesPrefix"
					label:        qsTr("Column prefix")
					defaultValue: "classProb"
					fieldWidth:   120
					enabled:      saveProbs.checked
					info:         qsTr("Columns are named prefix_C1, prefix_C2, …")
				}
			}

			CheckBox
			{
				id:    saveClass
				name:  "saveClassification"
				label: qsTr("Class classification")
				info:  qsTr("Saves the modal class assignment for each observation as a new nominal column.")

				TextField
				{
					name:         "saveClassificationColumn"
					label:        qsTr("Column name")
					defaultValue: "classAssignment"
					fieldWidth:   120
					enabled:      saveClass.checked
					info:         qsTr("Name of the column holding the most-likely class label for each observation.")
				}
			}
		}
		
	}
}
