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
import "./common" as Common

Form
{

	// Common.PcaEfaVariables{}
	VariablesForm
	{
		// property alias variables: variables

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

	Common.PcaEfaNumberFactors{
		pca: true
		variablesCount: variables.count
	}

	Common.PcaEfaAnalysisOptions{
		pca: true
		dataRaw: dataType.value == "raw"
		nonScale: variables.count > 0 && (variables.columnsTypes.includes("ordinal") || variables.columnsTypes.includes("nominal"))
	}

	Common.PcaEfaOutputOptions{
		pca: true
		dataRaw: dataType.value == "raw"
		variablesCount: variables.count

	}

}
