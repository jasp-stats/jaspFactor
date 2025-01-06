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

	id: analysisoptions
	title: qsTr("Analysis Options")
	expanded: true


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
		title: qsTr("Base Decomposition on")
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
			enabled: dataRaw
			value: "polyTetrachoricCorrelationMatrix"
			label: qsTr("Polychoric/tetrachoric correlation matrix")
		}
	}


	Group
	{
		visible: !pca
		title: qsTr("Factoring Method")
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