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
	property bool nonScale: false

	id: analysisoptions
	title: qsTr("Analysis Options")
	info: qsTr("Here, options for the analysis can be specified.")
	expanded: false


	RadioButtonGroup
	{
		name: "rotationMethod"
		title: qsTr("Rotation")
		info: qsTr("Here, the rotation method to apply to the components can be specified. Rotation ensures a simpler understanding of the data structure.")
		RadioButton
		{
			value	: "orthogonal"
			label	: qsTr("Orthogonal")
			info: qsTr("This method produces components that are uncorrelated. For this method, there are several possibilities that can be selected.")
			DropDown
			{
				name: "orthogonalSelector"
				values: [
					{ label: qsTr("none")	, value: "none"	, 		info: qsTr("No rotation method is selected.")},
					{ label: "varimax"		, value: "varimax", 	info: qsTr("Maximizes the variance of squared loadings for each factor, simplifying interpretations.")},
					{ label: "quartimax"	, value: "quartimax", info: qsTr("Minimizes the number of factors needed to explain each variable, simplifying structure.")	},
					{ label: "bentlerT"		, value: "bentlerT", 	info: qsTr("Orthogonal rotation method by Bentler, used for simplifying factor loadings.")},
					{ label: "equamax"		, value: "equamax", 	info: qsTr("Combines Varimax and Quartimax to balance factor simplicity and variable explanations.")},
					{ label: "geominT"		, value: "geominT", 	info: qsTr("Applies an orthogonal rotation minimizing a weighted sum of squared loadings.")}
				]
			}
		}
		RadioButton
		{
			value	: "oblique"
			label	: qsTr("Oblique")
			info: qsTr("This method produces components that allow for correlation between the components. This method is selected by default. Several possibilities are available. The default is promax.")
			checked	: true
			DropDown { 
				name: "obliqueSelector"
				values: [ 
					{ value: "promax", label: "promax", info: qsTr("Starts with Varimax and applies a power transformation to allow correlated factors.")},
					{ value: "oblimin", label: "oblimin", info: qsTr("An oblique rotation that minimizes the correlation among factors while allowing flexibility.")},
					{ value: "simplimax", label: "simplimax", info: qsTr("Aims to simplify factor loadings by minimizing nonzero entries.") },
					{ value: "bentlerQ", label: "bentlerQ", info: qsTr("Oblique rotation by Bentler, simplifying factor correlations.")},
					{ value: "biquartimin", label: "biquartimin", info: qsTr("An oblique rotation balancing between simple and interpretable factor loadings.")},
					{ value: "cluster", label: "cluster", info: qsTr("Targets a cluster rotation for factor analysis, emphasizing group separation.") },
					{ value: "geominQ", label: "geominQ", info: qsTr("An oblique version of Geomin, allowing factor correlations and reducing complex loadings.")}
				]
			}
		}
	}

	RadioButtonGroup
	{
		name: "analysisBasedOn"
		title: qsTr("Base Decomposition on")
		info: qsTr("What to base the decomposition of the data into components/factors on.")
		RadioButton
		{
			value: "correlationMatrix"
			label: qsTr("Correlation matrix")
			info: qsTr("The correlation matrix is used to decompose the data into components/factors.")
			checked: !nonScale
		}
		RadioButton
		{
			value: "covarianceMatrix"
			label: qsTr("Covariance matrix")
			info: qsTr("The covariance matrix is used to decompose the data into components/factors.")
		}
		RadioButton
		{
			enabled: dataRaw && nonScale
			value: "polyTetrachoricCorrelationMatrix"
			label: qsTr("Polychoric/tetrachoric correlation matrix")
			info: qsTr("The polychoric/tetrachoric correlation matrix is used to decompose the data into components/factors. This is sometimes unstable when sample size is small and when some variables do not contain all response categories")
			checked: nonScale
		}
	}


	Group
	{
		visible: !pca
		title: qsTr("Factoring Method")
		DropDown
		{
			name: "factoringMethod"
			info: qsTr("Which factoring method to use for the decomposition.")
			indexDefaultValue: 0
			values:
			[
				{ label: qsTr("Minimum residual"),			value: "minimumResidual", info: qsTr("Perform a minimum residual factor analysis (minres) using the first derivative.")},
				{ label: qsTr("Maximum likelihood"),		value: "maximumLikelihood", info: qsTr("Perform a maximum likelihood factor analysis (ml).")		},
				{ label: qsTr("Principal axis factoring"),	value: "principalAxis", info: qsTr("Perform a principal factor solution (pa).")			},
				{ label: qsTr("Ordinary least squares"),	value: "ordinaryLeastSquares", info: qsTr("Minimize the residual matrix using an OLS procedure, slower but uses the empirical first derivative.")	},
				{ label: qsTr("Weighted least squares"),	value: "weightedLeastSquares", info: qsTr("Perform a weighted least squares (WLS) solution.")	},
				{ label: qsTr("Generalized least squares"), value: "generalizedLeastSquares", info: qsTr("Perform a generalized least squares (GLS) solution.")},
				{ label: qsTr("Minimum chi-square"),		value: "minimumChiSquare", info: qsTr("Minimize the sample size-weighted chi-square using pairwise correlations.")		},
				{ label: qsTr("Minimum rank"),				value: "minimumRank", info: qsTr("Perform a minimum rank factor analysis (minrank).")		}
			]
		}
	}

}