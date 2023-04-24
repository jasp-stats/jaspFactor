Exploratory Factor Analysis 
=== 

With Exploratory Factor Analysis it is possible to identify one or more factors underlying the data. The factors are chosen such that they capture the common variance in the data.

### Assumptions (Yong & Pearce, 2013)
- The variables included in the analysis are continuous*.
  - If variables are ordinal, this assumption can be overcome when basing the analysis on the polychoric or tetrachoric correlation matrix (Bandalos & Finney, 2018).
- The data follow a multivariate normal distribution.
- There is a linear relation between the variables and the factors.
- There is no multicollinearity and singularity in the data.

### Input
---
#### Asssignment Box 
- Included Variables: In this box, the variables to perform the exploratory factor analysis on are selected. 

#### Number of Factors 
- _NB: eigenvalues for EFA are different from eigenvalues for PCA. See Dinno (2014) for more information._
- Here, the number of factors that the rotation is applied to is specified. Several methods to determine this number can be chosen from:   
  - Parallel Analysis: Factors are selected on the basis of parallel analysis. With this method, factors are selected when their eigenvalue is greater than the parallel average random eigenvalue. This method is selected by default. Can be based on principal component eigenvalues (PC) or factor eigenvalues (FA). A seed (1234) is chosen by default so that the results from the parallel analysis are equal across the EFA.
  - Eigenvalues: Factors are selected when they have a certain eigenvalue. By default factors are selected that have an eigenvalue of 0 or higher. This is called the Kaiser criterion. 
  - Manual: The number of factors can be specified manually. By default this is set to 1. 

#### Estimation Method:
- Choose the estimation method used within the psych package to find the factor solution, options are: 
  - Minimum residual (default), maximum likelihood, principal axis factoring, ordinal elast squares, 
  weighted least squares, generalized least squares, minimum chi-square, minimum rank

#### Rotation 
- Here, the rotation method to apply to the factors can be specified.
  - Orthogonal: This method produces factors that are uncorrelated. For this method, there are several possibilities that can be selected: 
      - None: No rotation method is selected. 
      - varimax: Orthogonal rotation method varimax. This rotation is based on maximizing the variance of the loadings. 
      - quartimax: Orthogonal rotation method quartimax. For this method, the number of factors that is necessary to explain each variable is minimized. 
      - bentlerT: Orthogonal rotation method bentlerT. 
      - equamax: Orthogonal rotation method equamax. This is a combination of varimax and quartimax. 
      - varimin: Orthogonal rotation method varimin. 
  - Oblique: This method produces factors that allow for correlation between the factors. This method is selected by default. Several possibilities are available: 
      - promax: Oblique rotation method promax. This method is selected by default. 
      - oblimin: Oblique rotation method oblimin. 
      - simplimax: Oblique rotation method simplimax. 
      - bentlerQ: Oblique rotation method bentlerQ. 
      - biquartimin: Oblique rotation method biquartimin. 
      - cluster: Oblique rotation method cluster. 

#### Base decomposition on
- Correlation: Bases the PCA on the correlation matrix of the data
- Covariance: Bases the PCA on the covariance matrix of the data
- Polychoric/tetrachoric: Bases the PCA on the poly/tetrachoric (mixed) correlation matrix of the data. This is sometimes unstable when sample size is small and when some variables do not contain all response categories.

### Output Options 
- Highlight: This option cuts the scaling of paths in width and color saturation. Paths with absolute weights over this value will have the strongest color intensity and become wider the stronger they are, and paths with absolute weights under this value will have the smallest width and become vaguer the weaker the weight. If set to 0, no cutoff is used and all paths vary in width and color.
- Include Tables: 
    - Factor correlations: When selecting this option, a table with the correlations between the factors will be displayed. 
    - Additional fit indices: This option displays the Root Mean Squared Error of Approximation (RMSEA) with 90% confidence interval, the Tucker Lewis Index (TLI), and the Bayesian Information Criterion (BIC) to test the fit of the model. 
    - Residual matrix: Displays a table containing the residual variances and correlations
    - Parallel analysis: If this option is selected, a table will be generated exhibiting a detailed output of the parallel analysis. Can be based on principal component eigenvalues (PC) or factor eigenvalues (FA). The seed is taken from the parallel analysis for determining the number of factors above
- Plots:
    - Path diagram: By selecting this option, a visual representation of the direction and strength of the relation between the variable and factor will be displayed. 
    - Scree plot: When selecting this option, a scree plot will be displayed. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. A scree plot can be used to decide how many factors should be selected.
      - Parallel analysis results: Display the results of the parallel analysis in the scree plot. The parallel analysis will be based on PC or FA as defined by the option for the parallel analysis table
- Assumption Checks:
  - Kaiser-Meyer-Olkin Test (KMO): Determines how well variables are suited for factor analysis by computing the proportion of common variance between variables
  - Bartlett's Test (of sphericity): Determines if the data correlation matrix is the identity matrix, meaning, if the variables are related or not
	- Mardia's Test of Multivariate Normality: Assesses the degree of the departure from multivariate normality of the included variables in terms of multivariate skewness and kurtosis. The Mardia's test will always include the listwise complete cases.
- Missing values: 
    - Exclude cases pairwise: If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have an observation for all the variables to include the case in the analysis. This option is selected by default. 
    - Exclude cases listwise: If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. 

### Output 
---
#### Assumption Checks
- Kaiser-Meyer-Olkin Test (KMO): Measure of sampling adequacy (MSA) as the proportion of common variance among variables is computed for all variables; values closer to 1 are desired.
- Bartlett's Test (of sphericity): A significant result means the correlation matrix is unlike the identity matrix.
- Mardia's Test of Multivariate Normality:
	- Tests: The first column shows all the tests performed.
	- Value: The values of `b1p` (multivariate skewness) and `b2p` (multivariate kurtosis), as denoted in Mardia (1970).
	- Statistic: The two chi-squared test statistics of multivariate skewness (both standard and corrected for small samples) and the standard normal test statistic of multivariate kurtosis.
	- df: Degrees of freedom.
	- p: P-value. 

#### Exploratory Factor Analysis
- Factor Loadings:
  - Variables: The first column shows all the variables included in the analysis. 
  - PC (1, 2, 3, etc.): This column shows the factor loadings on the variable. 
  - Uniqueness: The percentage of the variance of each variable that is not explained by the factor. 

- Factor Correlations:  
  - The correlations between the factors. 

- Chi-squared Test: 
  The fit of the model is tested. When the test is significant, the model is rejected. Bear in mind that a     chi-squared approximation may be unreliable for small sample sizes, and the chi-squared test may too         readily reject the model with very large sample sizes. Additional information about the fit of the model     can be obtained by selecting the option `Additional fit indices` in the `Output options`. See, for example,   Saris, Satorra, & van der Veld (2009) for more discussions on overall fit metrics.
  - Model: The model obtained from the exploratory factor analysis. 
  - Value: The chi-squared test statistic.  
  - df: Degrees of freedom. 
  - p: P-value. 

- Factor Characteristics:
  - Unrotated solution: 
    - SumSq. Loadings: Sum of squared loadings, variance explained by each unrotated factor
    - Proportion var.: The proportion of variance in the dataset explained by each unrotated factor
    - Cumulative: The proportion of variance in the dataset explained by the unrotated factor up to and including the current factor.
  - Rotated solution: 
  - SumSq. Loadings: Sum of squared loadings, variance explained by each rotated factor
  - Proportion var.: The proportion of variance in the dataset explained by each rotated factor
  - Cumulative: The proportion of variance in the dataset explained by the rotated factor up to and including the current factor.

- Additional Fit Indices: 
  These fit indices provide information about the fit of the model. 
  - Model: The model obtained from the exploratory factor analysis. 
  - RMSEA: Root Mean Square Error of Approximation. Corrects for parsimony. When models peform the same, but   model 1 has more degrees of freedom than model 2, model 1 will be recommended. Browne and Cudeck (1993) advise a value less than 0.08 for an acceptable model fit, less than 0.05 a good model fit, and advice to reject models with values of 0.1 or higher. However, there is absolute agreement on these cutoffs. 
  - RMSEA 90% confidence interval: The 90% confidence interval of the Root Mean Square Error of Approximation. 
  - SRMR: Standardized root mean square residual. Cutoffs similar to RMSEA
  - TLI: Tucker-Lewis Index. Evaluates the fit compared to a more resticted, nested baseline model. Hopwood and Donnallan (2010) suggested that a value higher than .9 indicates a good fit. However, there is no consensus about this cutoff. 
  - CFI: Comparative fit index. Cutoffs similar to TLI
  - BIC: Bayesian Information Criterion. This measure is useful for comparing the performances of different models on the same data, where a lower value indicates a better fitting model. 
  
- Parallel Analysis: The table displays as many factors as variables selected for analysis, eigenvalues corresponding to the real-data factor, and the eigenvalue corresponding to the parallel mean resampled value. It will display an asterisk along the names of the factors advised to be retained (whose real-data eigenvalue is greater than the resampled-data mean value). Note that, even when selecting a PC-based parallel analysis, the table will refer to "factors" as the ones advised to be retained instead of "components"; this is due to common usage of the PC-based parallel analysis method for assessing the number of factors within EFA (e.g., Golino et al., 2020).

#### Path Diagram 
- F(1,2,3,...): The factors in the model are represented by the circles.  
- Variables: The variables are represented by the boxes. 
- Arrows: Going from the factors to the variables, representing the loading from the factor on the variable. Red indicates a negative loading, green a positive loading. The wider the arrows, the higher the loading. This highlight can be adjusted at `Highlight` in the `Output Options`. 

#### Screeplot 
The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. The scree plot can be used to decide how many factors should be selected in the model. 

- Factors: On the x-axis, all possible factors. 
- Eigenvalue: On the y-axis, the eigenvalue that indicates the variance explained by each factor. 
- Data: The dotted line represents the data. 
- Simulated: The triangle line represents the simulated data. This line is indicative for the parallel analysis. When the points from the dotted line (real data) are above this line, these factors will be included in the model by parallel analysis. 
- Kaiser criterion: The horizontal line at the eigenvalue of 1 represents the Kaiser criterion. According to this criterion, only factors with values above this line (at an eigenvalue of 1) should be included in the model. 

### References 
---
- Bandalos, D. L., & Finney, S. J. (2018). Factor analysis: Exploratory and confirmatory. In G. R. Hancock, L. M. Stapleton, & R. O. Mueller, *The reviewer’s guide to quantitative methods in the social sciences* (pp. 98-122). Routledge. https://doi.org/10.4324/9781315755649
- Dinno, A. (2014) Gently clarifying the application of Horn’s parallel analysis to principal component analysis versus factor analysis. *Working paper*. https://alexisdinno.com/Software/files/PA_for_PCA_vs_FA.pdf
- Dziuban, C. D., & Shirkey, E. C. (1974). When is a correlation matrix appropriate for factor analysis? Some decision rules. *Psychological Bulletin, 81*(6), 358–361. https://doi.org/10.1037/h0036316
- Golino, H., Shi, D., Christensen, A. P., Garrido, L. E., Nieto, M. D., Sadana, R., ... & Martinez-Molina, A. (2020). Investigating the performance of exploratory graph analysis and traditional techniques to identify the number of latent factors: A simulation and tutorial. *Psychological Methods*, *25*(3), 292. https://doi.org/10.1037/met0000255
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention 
    decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational Research Methods, 7*(2), 191-205. https://doi.org/10.1177/1094428104263675
- Hopwood, C. J., & Donnellan, M. B. (2010). How should the internal structure 
    of personality inventories be evaluated? *Personality and Social Psychology Review, 14*(3), 332–346. https://doi.org/10.1177/1088868310361240
- Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. *Psychometrika, 30*(2), 179–185. https://doi.org/10.1007%2Fbf02289447
- Hu, L.-t., & Bentler, P. M. (1998). Fit indices in covariance structure modeling: Sensitivity to underparameterized model misspecification. *Psychological Methods, 3*(4), 424–453. https://doi.org/10.1037/1082-989X.3.4.424
- Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications. *Biometrika*, *57*(3), 519-530. https://doi.org/10.2307/2334770
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in 
    exploratory factor analysis. In J. Osborne (Ed.), *Best practices in quantitative methods* (pp. 86-99). SAGE Publications, Inc. https://doi.org/10.4135/9781412995627.d8
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. *Structural Equation Modeling: A Multidisciplinary Journal, 16*(4), 561-582. https://doi.org/10.1080/10705510903203433
- Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment of ordered polytomous items with parallel analysis. *Psychological Methods*, *16*(2), 209. https://doi.org/10.1037/a0023353
- Yong, A. G., & Pearce, S. (2013). A beginner’s guide to factor analysis: Focusing on exploratory factor analysis. *Tutorials in Quantitative Methods for Psychology*, *9*(2), 79-94. https://doi.org/10.20982/tqmp.09.2.p079

### R Packages 
---
- ggplot2
- psych
- qgraph
- stats

### Example 
---
- For an example go to `File`-->`Data library`-->`Factor`-->`G Factor`. 
- For more details about Exploratory Factor Analysis in JASP, watch this <a href="https://www.youtube.com/watch?v=dUPzMBqcMjo&feature=youtu.be">video</a>. 
