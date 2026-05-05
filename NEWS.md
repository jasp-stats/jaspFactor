# jaspFactor Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
<<<<<<< HEAD
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.

=======
>
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspFactor (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.).
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspFactor/issues/19))`).
> * **Format Categories:**
>   * **Added:** New features or output options.
>   * **Changed:** Updates to defaults, UI, or dependencies.
>   * **Fixed:** Bug fixes in R code, QML layouts, or output.
>   * **Deprecated / Removed:** Outdated components or legacy code.
>>>>>>> 82e39dd (adjust news)

---

# jaspFactor (development version)

<<<<<<< HEAD
=======
## Fixed
* EFA: polychoric/tetrachoric correlation matrix errors are now caught and shown as a user-friendly message, including which variables have missing response categories.
* EFA, PCA: ordinal variables with value labels (e.g., from SPSS) were incorrectly treated as having missing values when computing polychoric correlations; fixed by improving factor/ordered-to-numeric coercion ([jasp-issues#4129](https://github.com/jasp-stats/jasp-issues/issues/4129), [jasp-issues#4224](https://github.com/jasp-stats/jasp-issues/issues/4224)) ([PR #336](https://github.com/jasp-stats/jaspFactor/pull/336)).
* CFA: Chi-square table footnote now always reports the estimator, test statistic, and standard error method, making it clear which defaults lavaan applied (e.g., Browne.residual.nt when DWLS is used on continuous data) ([jasp-issues#4157](https://github.com/jasp-stats/jasp-issues/issues/4157), [jasp-issues#4171](https://github.com/jasp-stats/jasp-issues/issues/4171)) ([PR #336](https://github.com/jasp-stats/jaspFactor/pull/336)).

---

# jaspFactor 0.19.2
>>>>>>> 82e39dd (adjust news)

## Added
* CFA: added more estimators to allow robust estimation; ordinal data fully supported ([PR #235](https://github.com/jasp-stats/jaspFactor/pull/235)).
* CFA, EFA, PCA: added covariance matrix input option ([PR #239](https://github.com/jasp-stats/jaspFactor/pull/239)).

<<<<<<< HEAD
=======
## Changed
* CFA: standardized output restructured; standardization now also applied to bootstrapped estimates ([PR #235](https://github.com/jasp-stats/jaspFactor/pull/235)).

>>>>>>> 82e39dd (adjust news)
---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
