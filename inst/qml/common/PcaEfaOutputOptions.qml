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
		name: "loadingsOrder"
		title: qsTr("Order Loadings By")
		info: qsTr("Either order the loadings by their size from large to small, or by variables, meaning according to their occurence in the variables list.")
		RadioButton	{ name: "sortBySize";				label: qsTr("Size");	checked: true		}
		RadioButton	{ name: "sortByVariables";	label: qsTr("Variables")							}
	}

	Group
	{
		title: qsTr("Tables")
		info: qsTr("Display addition tables in the output.")
		CheckBox { visible: !pca; name: "factorStructure";	label: qsTr("Structure matrix"); info: qsTr("An item by factor structure matrix. This is just the loadings (pattern) matrix times the factor intercorrelation matrix.")}
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
		CheckBox {
			name:	"parallelAnalysisTable";
			label:	qsTr("Parallel analysis")
			info: qsTr("If this option is selected, a table will be generated exhibiting a detailed output of the parallel analysis. Can be based on principal component eigenvalues (PC) or factor eigenvalues (FA). The seed is taken from the parallel analysis for determining the number of components/factors above.")
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
		info: qsTr("Display plots.")
		CheckBox {
			name: "pathDiagram"
			label: qsTr("Path diagram")
			info: qsTr("By selecting this option, a visual representation of the direction and strength of the relation between the variable and factor will be displayed.")
		}
		CheckBox {
			name:  "screePlot";
			label: qsTr("Scree plot")
			info: qsTr("When selecting this option, a scree plot will be displayed. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. A scree plot can be used to decide how many factors should be selected.")

			CheckBox {
				name:		"screePlotParallelAnalysisResults"
				label:		qsTr("Parallel analysis results")
				checked:	true
				info: qsTr("Display the results of the parallel analysis in the scree plot. The parallel analysis will be based on PC or FA as defined by the option for the parallel analysis table.")
			}
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
		CheckBox { 
			name: "antiImageCorrelationMatrix"
			label: qsTr("Anti-image correlation matrix") 
			info: qsTr("Contains the negative partial correlations between pairs of variables after accounting for the effects of all other variables in the dataset. High values in the anti-image correlation matrix indicate that a variable might be redundant or have too much shared variance with other variables.")
		}
	}

	RadioButtonGroup
	{
		name: "naAction"
		title: qsTr("Missing Values")
		info: qsTr("Select how to handle missing values.")
		RadioButton { 
			value: "pairwise";		
			label: qsTr("Exclude cases pairwise"); 
			checked: true; 
			info: qsTr("If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have an observation for all the variables to include the case in the analysis. This option is selected by default.")
		}
		RadioButton { 
			value: "listwise";		
			label: qsTr("Exclude cases listwise")
			info: qsTr("If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. ")
		}
	}

	CheckBox
	{
		id: addScores
		name: "addScores"
		label: pca ? qsTr("Add PC scores to data") : qsTr("Add FA scores to data")
		info: qsTr("Adds the estimated component/factor scores as new columns to the data set")
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