context("Principal Component Analysis -- Verification project")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - slider

defaultOptions <- list(
  variables = list(),
  sampleSize = 200,
  eigenValuesAbove = 1,
  manualNumberOfComponents = 1,
  orthogonalSelector = "none",
  obliqueSelector = "promax",
  loadingsDisplayLimit = 0,
  componentCorrelations = FALSE,
  residualMatrix = FALSE,
  parallelAnalysisTable = FALSE,
  pathDiagram = FALSE,
  screePlot = FALSE,
  screePlotParallelAnalysisResults = TRUE,
  kaiserMeyerOlkinTest = FALSE,
  bartlettTest = FALSE,
  mardiaTest = FALSE,
  addScores = FALSE,
  addedScoresPrefix = "",
  dataType = "raw",
  componentCountMethod = "parallelAnalysis",
  parallelAnalysisMethod = "principalComponentBased",
  rotationMethod = "orthogonal",
  analysisBasedOn = "correlationMatrix",
  loadingsOrder = "sortByVariables",
  parallelAnalysisTableMethod = "principalComponentBased",
  naAction = "pairwise",
  plotWidth = 480,
  plotHeight = 320,
  setSeed = FALSE,
  seed = 1
)
options <- defaultOptions
## Testing Questionnaire data

# https://jasp-stats.github.io/jasp-verification-project/factor.html
options <- defaultOptions
options$PCPrefix <- ""
options$loadingsDisplayLimit <- 0.4
options$orthogonalSelector <- "varimax"
options$variables <- c(paste("Question", 1:9, sep="_0"), paste("Question", 10:23, sep="_"))
options$componentCountMethod <- "parallelAnalysis"
options$rotationMethod <- "orthogonal"
options$analysisBasedOn <- "correlationMatrix"
options$naAction <- "pairwise"
options$screePlot <- TRUE

set.seed(1)
results <- jaspTools::runAnalysis("principalComponentAnalysis", "PCA.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/factor.html
test_that("Chi-squared Test table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test"=resultsTable,
    "ref"=list(2634.37444035402, 167, "Model", 0)
  )
})


# https://jasp-stats.github.io/jasp-verification-project/factor.html
test_that("Component Loadings table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(
    "test"=resultsTable,
    "ref"=list("", 0.496602144100478, "", "", 0.565352292517859, "Question_01",
               "", "", "", 0.543539986797769, 0.586247492596422, "Question_02",
               "", -0.566460771824143, "", "", 0.470283972563951, "Question_03",
               "", 0.516500643569457, "", "", 0.531410988062391, "Question_04",
               "", 0.429500118113857, "", "", 0.656950179566024, "Question_05",
               0.799571915415243, "", "", "", 0.346068303111621, "Question_06",
               0.637943394282936, "", "", "", 0.454705735644512, "Question_07",
               "", "", 0.83306283668077, "", 0.260536467171317, "Question_08",
               "", "", "", 0.648445732164417, 0.515519535692423, "Question_09",
               0.550320339955909, "", "", "", 0.665227370979451, "Question_10",
               "", "", 0.746875564635845, "", 0.310395149829451, "Question_11",
               0.472456230652706, 0.52335749778436, "", "", 0.486671924069512,
               "Question_12", 0.64720761846246, "", "", "", 0.464171555549614,
               "Question_13", 0.578414819056257, "", "", "", 0.511735111298518,
               "Question_14", 0.458766639968431, "", "", "", 0.622008224462925,
               "Question_15", "", 0.514510271564887, "", "", 0.512917787132787,
               "Question_16", "", "", 0.74690563537787, "", 0.317191505144432,
               "Question_17", 0.683605318780917, "", "", "", 0.402662210858605,
               "Question_18", "", "", "", 0.428012727362413, 0.656757687466394,
               "Question_19", "", 0.676648974115391, "", "", 0.516003543654907,
               "Question_20", "", 0.660842907123443, "", "", 0.450093077372075,
               "Question_21", "", "", "", 0.645059343658527, 0.536455686315584,
               "Question_22", "", "", "", 0.585155978333205, 0.587808706972929,
               "Question_23")
  )
})

# https://jasp-stats.github.io/jasp-verification-project/factor.html
#' the values presented here are slightly different from the SPSS, SAS, and Minitab values
#' the reason for this is the optimization that just runs a bit shorter with the psych package we use for JASP
#' adjusting the optimization (define a later breakpoint) results in the SPSS values
test_that("Component Characteristics table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(resultsTable,
                                 list("Component 1", 0.162054332805079, 0.316958567983434, 3.72724965451683,
                                      7.29004706361899, 0.162054332805079, 0.316958567983434, "Component 2",
                                      0.307422479496941, 0.392559817846783, 3.34346737391282, 1.73882874685703,
                                      0.145368146691862, 0.0756012498633492, "Component 3", 0.418331823475735,
                                      0.449809884276163, 2.55091491151227, 1.31675152787573, 0.110909343978794,
                                      0.0572500664293797, "Component 4", 0.503166325737665, 0.503166325737664,
                                      1.95119355202438, 1.22719815361453, 0.0848345022619297, 0.0533564414615014
                                 ))
})
