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
import JASP
import JASP.Controls

Form
{
	property bool nonScale: variables.count > 0 && (variables.columnsTypes.includes("ordinal") || variables.columnsTypes.includes("nominal"))

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList
		{
			id: variables
			name: "variables"
			title: qsTr("Variables")
			allowedColumns: ["scale", "ordinal", "nominal"]
			allowTypeChange: true
			info: qsTr("In this box, the variables to perform the analysis on are selected")
		}

		RadioButtonGroup
		{
			name: "dataType"
			title: qsTr("Data")
			id: dataType
			columns: 2
			info: qsTr("Specifies whether the data is raw, meaning observations in rows and variables in columns, or whether the data is a variance-covariance matrix. For the latter, the sample size is required.")
			RadioButton { value: "raw"; label: qsTr("Raw"); checked: true }
			RadioButton
			{
				value: "varianceCovariance"; label: qsTr("Variance-covariance matrix")
				IntegerField { name: "sampleSize"; label: qsTr("Sample size"); defaultValue: 200 }
			}
		}
	}

	Group
	{
		title: qsTr("Parallel Analysis")
		info: qsTr("Parallel analysis compares the eigenvalues of the data to eigenvalues of simulated random data. Factors/components are suggested for retention when their eigenvalue is greater than the 95th percentile of the simulated eigenvalues.")

		RadioButtonGroup
		{
			name: "parallelAnalysisMethod"
			title: qsTr("Eigenvalues based on")
			info: qsTr("Whether the parallel analysis is based on principal component eigenvalues (PC) or factor eigenvalues (FA). PC-based parallel analysis is a common choice also when the goal is to determine the number of factors for a factor analysis.")

			RadioButton
			{
				value:		"principalComponentBased"
				label:		qsTr("Principal components")
				checked:	true
			}
			RadioButton
			{
				value:	"factorBased"
				label:	qsTr("Factors")
			}
		}

		SetSeed{}
	}

	Group
	{
		title: qsTr("Eigenvalue Criterion")
		DoubleField
		{
			name:			"eigenvaluesAbove"
			label:			qsTr("Eigenvalues above")
			defaultValue:	1
			decimals:		1
			info: qsTr("Threshold for the eigenvalue criterion: factors/components with an eigenvalue above this value are suggested for retention. The default of 1 corresponds to the Kaiser criterion. The eigenvalue type (principal component or factor eigenvalues) follows the parallel analysis method selected above.")
		}
	}

	RadioButtonGroup
	{
		name: "baseDecompositionOn"
		title: qsTr("Base Analysis on")
		info: qsTr("What type of correlation matrix to base the analysis on.")
		RadioButton
		{
			value: "correlationMatrix"
			label: qsTr("Correlation matrix")
			info: qsTr("The Pearson correlation matrix is used.")
			checked: !nonScale
		}
		RadioButton
		{
			enabled: dataType.value == "raw" && nonScale
			value: "polyTetrachoricCorrelationMatrix"
			label: qsTr("Polychoric/tetrachoric correlation matrix")
			info: qsTr("The polychoric/tetrachoric correlation matrix is used. This is sometimes unstable when sample size is small and when some variables do not contain all response categories")
			checked: nonScale
		}
	}

	Group
	{
		title: qsTr("Output")
		CheckBox
		{
			name:		"parallelAnalysisTable"
			label:		qsTr("Parallel analysis table")
			checked:	true
			info: qsTr("Displays a table with the real data eigenvalues and the mean eigenvalues of the simulated data. Factors/components suggested for retention are marked with an asterisk.")
		}
		CheckBox
		{
			name:		"screePlot"
			label:		qsTr("Scree plot")
			checked:	true
			info: qsTr("Displays a scree plot. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor/component. The horizontal line marks the eigenvalue threshold.")

			CheckBox
			{
				name:		"screePlotParallelAnalysisResults"
				label:		qsTr("Parallel analysis results")
				checked:	true
				info: qsTr("Display the mean eigenvalues of the simulated data from the parallel analysis in the scree plot.")
			}
		}
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		info: qsTr("Select how to handle missing values.")
		RadioButton
		{
			value: "pairwise";
			label: qsTr("Exclude cases pairwise");
			checked: true;
			info: qsTr("If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have an observation for all the variables to include the case in the analysis. This option is selected by default.")
		}
		RadioButton
		{
			value: "listwise";
			label: qsTr("Exclude cases listwise")
			info: qsTr("If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. ")
		}
	}

}
