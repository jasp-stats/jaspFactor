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
	info: qsTr("Here, the number of components/factors that are used in the analysis is determined. Several methods to determine this number can be chosen from:")

	RadioButtonGroup
	{
		name: pca ? "componentCountMethod" : "factorCountMethod"
		title: pca ? qsTr("Based on") : qsTr("Based on")
		RadioButton
		{
			value:      "parallelAnalysis";
			label:      qsTr("Parallel analysis");
			checked:    true
			info: qsTr("Components/factors are selected on the basis of parallel analysis. With this method, factors are selected when their eigenvalue is greater than the parallel average random eigenvalue. This method is selected by default. Can be based on principal component eigenvalues (PC) or factor eigenvalues (FA). A seed (1234) is chosen by default so that the results from the parallel analysis are equal across the PCA")

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
			value: "eigenValues"
			label: qsTr("Eigenvalues")
			info: qsTr("Components are selected when they have a certain eigenvalue. By default components are selected that have an eigenvalue above 1.")
			DoubleField {
				name:			"eigenValuesAbove"
				label:			qsTr("Eigenvalues above")
				defaultValue:	1
				decimals:		1
			}
		}
		RadioButton
		{
			value: "manual"
			label: qsTr("Manual")
			info: qsTr("The number of components can be specified manually. By default this is set to 1.")
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