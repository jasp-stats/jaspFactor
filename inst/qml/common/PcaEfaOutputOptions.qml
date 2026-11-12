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

	title: qsTr("Output Options")
	info: qsTr("Additional output options for the PCA/EFA analysis.")

	Slider
	{
		name: "loadingsDisplayLimit"
		label: qsTr("Display loadings above")
		info: qsTr("Loadings below this value will not be displayed in the output table.")
		value: 0.4
	}

	RadioButtonGroup
	{
		name: "loadingOrder"
		title: qsTr("Order Loadings By")
		info: qsTr("Either order the loadings by their size from large to small, or by variables, meaning according to their occurence in the variables list.")
		RadioButton	{ name: "size";				label: qsTr("Size");	checked: true		}
		RadioButton	{ name: "variables";	label: qsTr("Variables")							}
	}

	Group
	{
		title: qsTr("Tables")
		info: qsTr("Display addition tables in the output.")
		CheckBox { visible: !pca; name: "factorStructure";	label: qsTr("Structure matrix"); info: qsTr("An item by factor structure matrix. This is just the loadings (pattern) matrix times the factor intercorrelation matrix.")}
		CheckBox
		{
			visible: !pca
			name: "schmidLeimanTransformation"
			label: qsTr("Schmid-Leiman transformation")
			info: qsTr("Transforms the oblique factor solution into a hierarchical solution with a general factor (g), which all variables load on, and group factors that are orthogonal to the general factor. The loadings of the variables on the general factor and on the residualized group factors are displayed, together with the communalities (h\u00B2), the uniquenesses (u\u00B2), and the proportion of the common variance that is due to the general factor (p\u00B2). The general factor is the first factor of a factor analysis of the factor correlations of the oblique solution. This needs an oblique rotation and at least three factors (Schmid & Leiman, 1957).")
		}
		CheckBox { 								
			name: 	pca ? "componentCorrelations" : "factorCorrelations";		
			label: 	pca ? qsTr("Component correlations") : qsTr("Factor correlations")
			info: 	qsTr("When selecting this option, a table with the correlations between the components/factors will be displayed.")
		}
		CheckBox { 
			visible: !pca; 
			name: "fitIndices";			
			label: qsTr("Additional fit indices")
			info: qsTr("This option displays the Root Mean Squared Error of Approximation (RMSEA) with 90% confidence interval, the Tucker Lewis Index (TLI), and the Bayesian Information Criterion (BIC) to test the fit of the model.")
		}
		CheckBox { name: "residualMatrix";	label: qsTr("Residual matrix"); info: qsTr("Displays a table containing the residual variances and correlations.")}
	}
	Group
	{
		title: qsTr("Plots")
		info: qsTr("Display plots.")
		CheckBox
		{
			name: "mahalanobisOutlierPlot"
			label: qsTr("Mahalanobis distance Q-Q plot")
			enabled: dataRaw
			info: qsTr("Displays a Q-Q plot of the Mahalanobis distances (D\u00B2) against the quantiles of the chi-square distribution, as the function outlier of the R package psych does. Cases above the diagonal are further from the centre than expected; at most the five worst cases with a p-value below the value specified for the multivariate outliers (0.001 by default) are labelled with their case number.")
		}
		CheckBox {
			name: "pathDiagram"
			label: qsTr("Path diagram")
			info: qsTr("By selecting this option, a visual representation of the direction and strength of the relation between the variable and factor will be displayed.")
		}
	}

	Group
	{
		title: qsTr("Assumption checks")
		info: qsTr("Assumptions: The variables included in the analysis are correlated; the variables included in the analysis are linearly related (Shlens, 2014)")
		CheckBox { 
			name: "kaiserMeyerOlkinTest";	
			label: qsTr("KMO test"); 
			info: qsTr("Determines how well variables are suited for factor analysis by computing the proportion of common variance between variables. Produces a measure of sampling adequacy (MSA) as the proportion of common variance among variables is computed for all variables; values closer to 1 are desired.")
			}
		CheckBox { 
			name: "bartlettTest";			
			label: qsTr("Bartlett's test")	
			info: qsTr("Determines if the data correlation matrix is the identity matrix, meaning, if the variables are related or not. A significant result means the correlation matrix is unlike the identity matrix.")
		}
		CheckBox { 
			name: "mardiaTest";				
			label: qsTr("Mardia's test")
			enabled: dataType.value == "raw" 
			info: qsTr("Assesses the degree of the departure from multivariate normality of the included variables in terms of multivariate skewness and kurtosis. The Mardia's test will always include the listwise complete cases.")
		}
		CheckBox
		{
			name: "mahalanobisOutlier"
			label: qsTr("Multivariate outliers (Mahalanobis distance)")
			enabled: dataRaw
			info: qsTr("Displays the cases that are multivariate outliers, based on the Mahalanobis distance (D\u00B2) to the centre of the variables, as computed by the function outlier of the R package psych. Only the cases without missing values are used. The distances are compared to a chi-square distribution with as many degrees of freedom as there are variables, and cases with a p-value below the specified value are displayed.")
			DoubleField
			{
				name: "mahalanobisOutlierPValue"
				label: qsTr("p-value below")
				defaultValue: 0.001
				min: 0
				max: 1
				decimals: 3
				info: qsTr("Cases with a p-value of the Mahalanobis distance below this value are displayed as outliers.")
			}
		}
		CheckBox { 
			name: "antiImageCorrelationMatrix"
			label: qsTr("Anti-image correlation matrix") 
			info: qsTr("Contains the negative partial correlations between pairs of variables after accounting for the effects of all other variables in the dataset. High values in the anti-image correlation matrix indicate that a variable might be redundant or have too much shared variance with other variables.")
		}
	}

	CheckBox
	{
		id: addScores
		name: "scoreSavedToData"
		label: pca ? qsTr("Add PC scores to data") : qsTr("Add FA scores to data")
		info: qsTr("Adds the estimated component/factor scores as new columns to the data set. The scores are regression scores.")
		enabled: variablesCount > 1 & dataRaw

		TextField {
			name: "scoreSavedToDataPrefix"
			label: qsTr("Prefix")
			defaultValue: pca ? qsTr("PC") : qsTr("FA")
			fieldWidth: 80
			enabled: addScores.checked
		}

		DropDown
		{
			name: "scoreSavedToDataImputation"
			label: qsTr("Impute missing values by")
			enabled: addScores.checked
			info: qsTr("Select whether missing values are replaced by the mean or the median of the variable when computing the saved scores, like the impute option of the R package psych, so that cases with missing values also get a score. This only affects the saved scores, not the analysis. In contrast to psych the scores are not rescaled, so the scores of the cases without missing values do not change. Cases without any observed value do not get a score. It is only used if missing values are excluded pairwise, because with listwise deletion the cases with missing values are not part of the analysis.")
			values: [
				{ label: qsTr("None"),    value: "none",   info: qsTr("Cases with a missing value do not get a score.") },
				{ label: qsTr("Mean"),    value: "mean",   info: qsTr("Missing values are replaced by the mean of the variable.") },
				{ label: qsTr("Median"),  value: "median", info: qsTr("Missing values are replaced by the median of the variable.") }
			]
		}
	}

}