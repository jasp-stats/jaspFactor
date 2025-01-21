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
				id: dataType
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


	Common.NumberFactors{
		pca: true
		variablesCount: variables.count
	}

	Common.AnalysisOptions{
		pca: true
		dataRaw: dataType.value == "raw"
	}

	Common.OutputOptions{
		pca: true
		dataRaw: dataType.value == "raw"
		variablesCount: variables.count

	}

}
