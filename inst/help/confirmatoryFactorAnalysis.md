Confirmatory Factor Analysis
==========================

Confirmatory factor analysis (CFA) models observed variables (indicators) as noisy manifestations of underlying latent variables (factors). JASP's CFA is built on `lavaan` (lavaan.org; Rosseel, 2012), an `R` package for performing structural equation modeling. See Brown (2014) or Kline (2015) for books on the topic of CFA.

### Assignment Box
-------
In the assignment box, continuous and ordinal variables in your dataset can be assigned to different factors.
There is a minimum of one factor, and each factor needs to have at least two indicators. You can add factors by pressing the (+) button and remove factors by pressing the (-) button. You may rename factors by changing the name above the assignment boxes. Either scale or ordinal variables are allowed. 

If you use any ordinal variables, the chosen estimator will by default be "DWLS" and the test statistic and fit measures will be scaled and shifted (mean and variance adjusted, see *lavaan* documentation). Other possible estimators are "WLS" and "ULS". When you use ordinal variables it may make sense to choose robust standard errors.

### Second-order factor
-------
JASP allows the factors in turn to be indicators of a second-order factor. This can be specified by dragging the factor names to the second-order assignment box. Any factors not assigned as indicators of the second-order factor will be allowed to covary with each other and with the second-order factor, but not with the indicators of the second-order factor.

### Model options
-------
- Include mean structure: display means for the indicators and, in the case of multi-group CFA, the means of the latent variables. To identify the model, the following options can be selected:
  - Fix latent intercepts to zero
  - Fix manifest intercepts to zero
  - Fix mean of manifest intercepts to zero: translates to effect coding
- Assume factors uncorrelated: set the correlation between the different latent variables to 0.
- Factor scaling: factors can be given a scale in one of three ways:
  - Factor variances (default): the factor has a fixed variance of 1
  - Marker variable: the factor has the same scale as its first indicator as its factor loading is fixed to 1
  - Effects coding: the mean of the factor loadings is fixed to 1. For more information on the interpretation of effects coding, see Veen (2018)
- Residual covariances: to allow for covariance between indicators not explained by their respective factor, for example because questions were phrased in a similar way, drag two variables to the right-side assignment box.

### Multigroup CFA
------
- Grouping variable: Select a categorical variable here to create CFA models for each group 
- Invariance testing: Select a level of constraining parameters over the different groups.
  - configural: the different groups have the same CFA structure
  - metric: same as configural and the factor loadings are constrained to be equal across groups
  - scalar: same as metric and the means of the indicators (intercepts) are constrained to be equal across groups
  - strict: same as scalar and the residual variances, and residual covariances are constrained to be equal across groups
  - structural: same as strict and the latent means, variances, and covariances are constrained to be equal across groups

### Additional output
-------
- Additional fit measures: select these to display the value of the various fit indices in the output pane
- Kaiser-Meyer-Olkin (KMO) test: show Kaiser-Meyer-Olkin (KMO) test results for sampling adequacy (MSA).
- Bartlett's test of sphericity: show Bartlett's test of sphericity.
- R-Squared: the proportion of variance in the indicators that is explained by all predictors (factors)
- Average variance extracted (AVE): a measure of the amount of variance that is captured by a construct in relation to the amount of variance due to measurement error
- Heterotrait-monotrait ratio (HTMT): measure of the similarity between latent variables, assessment of discriminant validity. If the HTMT is clearly smaller than one, discriminant validity can be regarded as established.
- Reliability: a measure for the relative amount of test score variance that is explained by the factor(s). Output contains coefficient omega (McDonald's) for each indicator group, in total, and, if a second order factor is specified, gives omega_hierarchical. Also provides coefficient alpha (Cronbach's). Note the total test score variance in the denominator of the reliability equation is the observed test score variance, that is, the summed observed covariance matrix.
- Implied covariance matrix: show the covariance matrix implied by the model
- Residual covariance matrix: show the covariances between the indicators that remains after taking into consideration the model. A perfect model shows all 0s here. 
- Modification indices: Display MIs with a minimum cutoff. A MI shows how much the chi-square value of overall fit would change if the parameter in question is freed up. EPC shows the expected change of the parameter itself.
- Show lavaan syntax: Display the lavaan modeling syntax that would be needed to run the model in R


### Plots
-------
- Misfit plot: visually show the residual correlations (standardised residual covariance matrix) of the indicators
- Model plot: graphically show the estimated model structure

### Advanced
-------
- Emulation: Emulate results from different software
- Error calculation: Change how standard errors for the parameters are computed, if bootstrap the CIs are percentile bootstrap type
- Estimator: change the estimator for the CFA (Auto: ML if only scale variables used, WLS otherwise)
- Standardization: display standardized parameters for different standardization schemes
- Missing data handling: change the missing data handling method.

### References
-------
- Brown, T. A. (2014). *Confirmatory factor analysis for applied research*. Guilford Press. 
- Henseler, J., Ringle, C. M. & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. *Journal of the Academy of Marketing Science, 43*, 115–135 (2015). https://doi.org/10.1007/s11747-014-0403-8
- Kline, R. B. (2015). *Principles and practice of structural equation modeling* (4th ed.). Guilford Press.
- Rogers, P. (2024). Best practices for your confirmatory factor analysis: A JASP and *lavaan* tutorial. *Behavior Research Methods, 56*, 6634–6654. https://doi.org/10.3758/s13428-024-02375-7
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software, 48*(2), 1-36. https://doi.org/10.18637/jss.v048.i02


### R Packages
---
- ggplot2
- lavaan
- reshape2
- semPlot
- stats
