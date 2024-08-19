
### Help on Estimators
Some of the estimators come with options that are set in the background:

- Estimators without extra effects (standard errors and model test are standard):
	- ML, GLS, WLS, ULS, DWLS, DLS

- Extensions of ML-estimators with extra effects: 
	- MLM: classic robust se (se="robust.sem"), Satorra-Bentler test statistic (test="satorra.bentler")
	- MLMV: classic robust se, scaled and shifted test statistic (test="scaled.shifted")
	- MLMVS: classic robust se, mean and var adjusted Satterthwaite style test statistic (test="mean.var.adjusted")
	- MLF: first-order standard se (information="first.order"), standard test
	- MLR: Huber-White robust se (se="robust.huber.white"), Yuan-Bentler T2-star test statistic (test="yuan.bentler.mplus")

- Others: 
	- WLSM: implies DWLS with scaled test and robust se
	- WLSMV: implies DWLS with mean and var adjusted test and robust se
	- ULSM: implies ULS with scaled test and robust se
	- ULSMV: implies ULS with mean-var adjusted test and robust se

- Note: If you specify "Standard errors" instead of leaving it at default the corresponding options set by the estimators in the background will be overwritten