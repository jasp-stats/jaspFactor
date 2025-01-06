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
	property int variablesCount: 1

	id: numberof
	title: 	pca ? qsTr("Number of Components") : qsTr("Number of Factors")
	expanded: true


	RadioButtonGroup
	{
		name: pca ? "componentCountMethod" : "factorCountMethod"
		title: pca ? qsTr("Number of Components Based on") : qsTr("Number of Factors Based on")
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
			
			SetSeed{}

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
				name:						pca ? "manualNumberOfComponents" : "manualNumberOfFactors"
				label:					pca? qsTr("Number of components") : qsTr("Number of factors")
				defaultValue:		1
				min:						1
				max: 						variablesCount > 1 ? variablesCount : 1

			}
		}
	}

}