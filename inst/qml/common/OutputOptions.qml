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
import QtQuick.Layouts
import JASP
import JASP.Controls

Section
{
	property bool pca: true
	property bool dataRaw: true
	property int variablesCount: 1

	property string correlationsName: pca ? "componentCorrelations" : "factorCorrelations"
	property string correlationsLabel: pca ? qsTr("Component correlations") : qsTr("Factor correlations")


	title: qsTr("Output Options")

	Slider
	{
		name: "loadingsDisplayLimit"
		label: qsTr("Display loadings above")
		value: 0.4
	}

	RadioButtonGroup
	{
		name: "loadingsOrder"
		title: qsTr("Order Loadings By")
		RadioButton	{ name: "sortBySize";				label: qsTr("Size");	checked: true		}
		RadioButton	{ name: "sortByVariables";	label: qsTr("Variables")							}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { visible: !pca; name: "factorStructure";	label: qsTr("Structure matrix")				}
		CheckBox { 								
			name: 	pca ? "componentCorrelations" : "factorCorrelations";		
			label: 	pca ? qsTr("Component correlations") : qsTr("Factor correlations")	
		}
		CheckBox { visible: !pca; name: "fitIndices";				label: qsTr("Additional fit indices")	}
		CheckBox { 								name: "residualMatrix";									label: qsTr("Residual matrix")					}
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

	Group
	{
		title: qsTr("Assumption checks")
		CheckBox { name: "kaiserMeyerOlkinTest";	label: qsTr("KMO test")					}
		CheckBox { name: "bartlettTest";			label: qsTr("Bartlett's test")	}
		CheckBox { name: "mardiaTest";				label: qsTr("Mardia's test")	  ; enabled: dataType.value == "raw" }
		CheckBox { name: "antiImageCorrelationMatrix"; label: qsTr("Anti-image correlation matrix") }
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
		id: addScores
		name: "addScores"
		label: qsTr("Add PC scores to data")
		enabled: variablesCount > 1 & dataRaw

		TextField {
			name: "addedScoresPrefix"
			label: qsTr("Prefix")
			defaultValue: pca ? qsTr("PC") : qsTr("FA")
			fieldWidth: 80
			enabled: addScores.checked
		}
	}

}