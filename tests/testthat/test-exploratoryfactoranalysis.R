context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation

defaultOptions <- list(
  variables = list(),
  sampleSize = 200,
  manualNumberOfFactors = 1,
  factoringMethod = "minimumResidual",
  rotationMethodOrthogonal = "none",
  rotationMethodOblique = "promax",
  loadingsDisplayLimit = 0,
  factorStructure = FALSE,
  schmidLeimanTransformation = FALSE,
  factorCorrelations = FALSE,
  fitIndices = FALSE,
  residualMatrix = FALSE,
  pathDiagram = FALSE,
  antiImageCorrelationMatrix = FALSE,
  kaiserMeyerOlkinTest = FALSE,
  bartlettTest = FALSE,
  mardiaTest = FALSE,
  mahalanobisOutlier = FALSE,
  mahalanobisOutlierPValue = 0.001,
  mahalanobisOutlierPlot = FALSE,
  scoreSavedToData = FALSE,
  scoreSavedToDataPrefix = "",
  scoreSavedToDataImputation = "none",
  dataType = "raw",
  rotationMethod = "orthogonal",
  baseDecompositionOn = "correlationMatrix",
  loadingOrder = "variables",
  naAction = "pairwise",
  plotWidth = 480,
  plotHeight = 320
)

options <- defaultOptions
options$factoringMethod <- "minimumResidual"
options$loadingsDisplayLimit <- 0.4
options$factorCorrelations <- TRUE
options$fitIndices <- TRUE
options$pathDiagram <- TRUE
options$factorStructure <- TRUE
options$residualMatrix <- TRUE
options$manualNumberOfFactors <- 2
options$rotationMethodOblique <- "geominQ"
options$rotationMethod <- "oblique"
options$loadingOrder <- "variables"
options$variables <- c("contWide", "contcor1", "contcor2", "facFifty", "contExpon",
                          "debCollin1", "debEqual1")
set.seed(1)
results <- jaspTools::runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = F)


test_that("Factor Correlations table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, -0.0736228, "Factor 1", -0.0736228, 1, "Factor 2"))
})

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.211560139238448, 0.214456283799968, 1.765453969821246,
                                      0.211560139238448, 0.214456283799968, 1.480920974669139, 1.501193986599778,
                                      "Factor 2", 0.366100386049262, 0.366100386049289, 1.310153052198495,
                                      0.15454024681081, 0.15164410224932, 1.081781727675693, 1.061508715745243))
})

test_that("Additional fit indices table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-32.7898349547043, 1, 0, "0 - 0.065", 0.0303448017639664, 1.20127892716092
                                 ))
})
test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(4.05152653321549, 8, "Model", 0.85244487039262))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("", "", 0.951432334368898, "contWide", 0.654092561077089, "",
                           0.57413710933047, "contcor1", 1.00020594814694, "", -0.00255707470903843,
                           "contcor2", "", "", 0.953108905280117, "facFifty", "", 0.997800455330077,
                           0.00428892032601724, "contExpon", "", "", 0.998135387367175,
                           "debCollin1", "", "", 0.958751715702742, "debEqual1"))
})

test_that("Residual Matrix table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contWide", 0.951432334370607, 0.0112373371683486, -0.00454295818756886,
                                      -0.0148706449107472, -0.00519428394442353, -0.125257443372847,
                                      0.020275526281743, "contcor1", 0.0112373371683486, 0.57413710932081,
                                      0.00471350991178054, 0.0263330466494297, -0.0127263359328001,
                                      0.00285880595202862, 0.0382189272536737, "contcor2", -0.00454295818756886,
                                      0.00471350991178077, -0.0025570747054946, -0.0178203074176956,
                                      0.0108200520284527, 0.0172047845713419, -0.0387548561505903,
                                      "facFifty", -0.0148706449107472, 0.0263330466494297, -0.0178203074176956,
                                      0.9531089052801, 0.0141245292541732, -0.00176631106058231, -0.0632960313074266,
                                      "contExpon", -0.00519428394442353, -0.0127263359328001, 0.0108200520284527,
                                      0.0141245292541732, 0.00428892032584216, 0.0201554960330016,
                                      0.00347754876308279, "debCollin1", -0.125257443372847, 0.00285880595202862,
                                      0.0172047845713419, -0.00176631106058231, 0.0201554960330016,
                                      0.998135387367303, 0.00898031463684731, "debEqual1", 0.020275526281743,
                                      0.0382189272536737, -0.0387548561505903, -0.0632960313074266,
                                      0.00347754876308276, 0.00898031463684731, 0.958751715702019
                                 ))
})

test_that("Path Diagram plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_path"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "path-diagram")
})

test_that("Factor Loadings (Structure Matrix) table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_structureTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("", "", "contWide", 0.651914307711847, "", "contcor1", 1.00118914256335,
                           "", "contcor2", "", "", "facFifty", "", 0.997852981864278, "contExpon",
                           "", "", "debCollin1", "", "", "debEqual1"))
})

test_that("Missing values works", {
  options <- defaultOptions
  options$variables <- c("contNormal", "contGamma", "contcor1", "debMiss30")
  options$factorCorrelations <- TRUE

  options$naAction <- "pairwise"
  results <- jaspTools::runAnalysis("exploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("Model", 1.42781053334818, 2L, 0.489727939944839), label = "pairwise")

  options$naAction <- "listwise"
  results <- jaspTools::runAnalysis("exploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("Model", 0.491396758561133, 2L, 0.782158104440787), label = "listwise")
})



options$loadingOrder <- "variables"

test_that("loadingOrder sort the factor loadings table", {

  options <- defaultOptions
  options$manualNumberOfFactors <- 3 # as previously suggested by parallel analysis
  options$rotationMethodOrthogonal <- "varimax"
  options$loadingsDisplayLimit <- 0.2
  options$variables <- paste0("x", 1:9)

  reference <- list(
    size = list(
      0.859092745287366, "", "", 0.246269423771584, "x5", 0.832032514222841,
      "", "", 0.27206434823954, "x4", 0.798985079015342, 0.213747389751454,
      "", 0.308644745749111, "x6", 0.279356098904291, 0.612821903382293,
      "", 0.523245992219826, "x1", "", 0.659769507411633, "", 0.546549851738645,
      "x3", "", 0.493809804203504, "", 0.744772821204617, "x2", "",
      0.414642352102081, 0.521227164426046, 0.539538616482869, "x9",
      "", "", 0.709321543667037, 0.481445404925259, "x7", "", "",
      0.698776373366381, 0.479827748357628, "x8"
    ),
    variables = list(
      0.279356098904291, 0.612821903382293, "", 0.523245992219826, "x1",
      "", 0.493809804203504, "", 0.744772821204617, "x2", "", 0.659769507411633,
      "", 0.546549851738645, "x3", 0.832032514222841, "", "", 0.27206434823954,
      "x4", 0.859092745287366, "", "", 0.246269423771584, "x5", 0.798985079015342,
      0.213747389751454, "", 0.308644745749111, "x6", "", "", 0.709321543667037,
      0.481445404925259, "x7", "", "", 0.698776373366381, 0.479827748357628,
      "x8", "", 0.414642352102081, 0.521227164426046, 0.539538616482869,
      "x9"
    )
  )

  for (loadingOrder in c("size", "variables")) {
    options$loadingOrder <- loadingOrder

    set.seed(123)
    results <- runAnalysis("exploratoryFactorAnalysis", "holzingerswineford.csv", options)

    table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
    jaspTools::expect_equal_tables(table, reference[[loadingOrder]], label = sprintf("loadingOrder = %s", loadingOrder))
  }

})

test_that("Estimation options do not crash", {
  options <- defaultOptions
  options$variables <- paste0("Q0", 1:9)

  for(factoringMethod in c("minimumResidual",
                           "maximumLikelihood",
                           "principalAxis",
                           "ordinaryLeastSquares",
                           "weightedLeastSquares",
                           "generalizedLeastSquares",
                           "minimumChiSquare",
                           "minimumRank")) {
    options$factoringMethod <- factoringMethod
    results <- runAnalysis("exploratoryFactorAnalysis", "Fear of Statistics.csv", options)
    testthat::expect(is.null(results[["results"]][["error"]]),
                     sprintf("Estimation with method '%s' crashes", factoringMethod))
  }
})


# 3 factors as previously suggested by PC-based parallel analysis
options <- defaultOptions
options$manualNumberOfFactors <- 3
options$factoringMethod <- "minimumResidual"
options$loadingsDisplayLimit <- 0.1
options$factorCorrelations <- TRUE
options$rotationMethodOrthogonal <- "none"
options$rotationMethod <- "orthogonal"
options$variables <- paste0("x", 1:9)
set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.314163647589992, 3.216344181437706, 0.314163647589992, 2.827472828309932,
                                      "Factor 2", 0.449126418894596, 1.638713221526056, 0.134962771304604, 1.214664941741433,
                                      "Factor 3", 0.539737894145641, 1.365159347786247, 0.090611475251045, 0.815503277259405
                                 ))
})

test_that("Chi-squared Test table results match with 3 factors", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5550491736693, 12, "Model", 0.0317499059313278))
})

test_that("Factor Loadings table results match with 3 factors", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.57552287197933, 0.16858001566767, 0.342210768279291, 0.523245992219849,
                                      "x1", 0.308426854688606, "", 0.388394269653172, 0.744772821204661,
                                      "x2", 0.400354070864591, 0.30942956544643, 0.444319828761981,
                                      0.546549851738708, "x3", 0.768508957341882, -0.354895458997614,
                                      -0.106671680536983, 0.272064348239582, "x4", 0.750543052745831,
                                      -0.404369058055611, -0.164016362264787, 0.246269423771613, "x5",
                                      0.763025590432945, -0.326669752718025, "", 0.308644745749135,
                                      "x6", 0.307604731575466, 0.432831609311044, -0.486405923245351,
                                      0.481445404925434, "x7", 0.393821088746758, 0.540664836861347,
                                      -0.269738272928661, 0.479827748357475, "x8", 0.504966416074441,
                                      0.453220919172841, "", 0.539538616482753, "x9"))
})

# 2 factors as previously suggested by PC-based parallel analysis
options <- defaultOptions
options$manualNumberOfFactors <- 2
options$loadingsDisplayLimit <- 0.1
options$baseDecompositionOn <- "polychoricTetrachoricCorrelationMatrix"
options$mardiaTest <- TRUE
options$kaiserMeyerOlkinTest <- TRUE
options$antiImageCorrelationMatrix <- TRUE
options$rotationMethod <- "oblique"
options$factoringMethod <- "minimumResidual"
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive","contNormal", "debMiss1")
options$variables.types <- list("scale", "scale", "scale", "ordinal", "scale", "scale")

set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = F)

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.23744462193867, 0.23818022169266, 1.78311572348898,
                                      0.23744462193867, 0.23818022169266, 1.42466773163202, 1.42908133015596,
                                      "Factor 2", 0.411139518227585, 0.411139518227585, 1.289241168930776,
                                      0.173694896288915, 0.172959296534925, 1.042169377733492, 1.037755779209552))
})

test_that("Mardia's Test of Multivariate Normality table results match with poly cor", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_mardiaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.0201435883819, 56, 0.706100154506541, 49.8323692083014, "Skewness",
                                      3.0201435883819, 56, 0.635010590136702, 51.78632377773, "Small Sample Skewness",
                                      44.9365639232773, 0.119834882997249, -1.55546702109016, "Kurtosis"
                                 ))
})

# might have to change this once psych is updated on CRAN so the diagonals are replced with MSA values
test_that("Anti-Image Correlation Matrix table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_antiMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contcor1", 1, "", "", "", "", "", "contcor2", -0.657984850645737,
                                      1, "", "", "", "", "facFifty", -0.0707897707888168, 0.19971424893446,
                                      1, "", "", "", "facFive", -0.0333887919581817, -0.0581760551284757,
                                      -0.129703921537719, 1, "", "", "contNormal", -0.223870987183178,
                                      0.139339768605635, 0.126683690677884, -0.131651354480395, 1,
                                      "", "debMiss1", 0.17345946187078, -0.0731362735186656, 0.00567413704057278,
                                      0.142797986754177, -0.26988982715786, 1))
})

test_that("Kaiser-Meyer-Olkin Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_kmoTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Overall MSA", 0.472338374588124, "contcor1", 0.490151695858873,
                                      "contcor2", 0.49029987989214, "facFifty", 0.473417708976819,
                                      "facFive", 0.515909446356412, "contNormal", 0.364284277677057,
                                      "debMiss1", 0.421070371345991))
})


# variance covariance matrix input
# this test fails because the columnIndexInData function does not play well with jaspTools
dt <- read.csv(testthat::test_path("holzingerswineford.csv"))
cdt <- as.data.frame(cov(dt[, 7:15]))
options <- list(
  scoreSavedToData = FALSE,
  scoreSavedToDataPrefix = "FA",
  scoreSavedToDataImputation = "none",
  baseDecompositionOn = "correlationMatrix",
  bartlettTest = FALSE,
  dataType = "varianceCovariance",
  factorCorrelations = FALSE,
  loadingOrder = "variables",
  factorStructure = FALSE,
  schmidLeimanTransformation = FALSE,
  factoringMethod = "minimumResidual",
  fitIndices = FALSE,
  kaiserMeyerOlkinTest = FALSE,
  loadingsDisplayLimit = 0.1,
  manualNumberOfFactors = 1,
  mardiaTest = FALSE,
  mahalanobisOutlier = FALSE,
  mahalanobisOutlierPValue = 0.001,
  mahalanobisOutlierPlot = FALSE,
  naAction = "pairwise",
  rotationMethodOblique = "promax",
  rotationMethodOrthogonal = "none",
  pathDiagram = FALSE,
  plotHeight = 320,
  plotWidth = 480,
  residualMatrix = FALSE,
  rotationMethod = "orthogonal",
  sampleSize = 200,
  antiImageCorrelationMatrix = FALSE,
  variables = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
)

set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", cdt, options, makeTests = F)

test_that("Factor Characteristics table results match", {
  skip("Skip test because columnIndexInData does not play nice with jaspTools yet")
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
   jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.292468456392298, 3.216344181437708, 0.292468456392298, 2.632216107530681
                                 ))
})

test_that("Chi-squared Test table results match", {
  skip("Skip test because columnIndexInData does not play nice with jaspTools yet")
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(235.832782509408, 27, "Model", 3.1571356942318e-35))
})

test_that("Factor Loadings table results match", {
  skip("Skip test because columnIndexInData does not play nice with jaspTools yet")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.558950485489651, 0.687574354770883, "x1", 0.30045377286727,
                                      0.909727530369823, "x2", 0.365159382803395, 0.866658625150644,
                                      "x3", 0.761746187230209, 0.41974274624024, "x4", 0.723809404128654,
                                      0.476099946494923, "x5", 0.768996689601407, 0.408644091382077,
                                      "x6", 0.25910333096785, 0.932865463881365, "x7", 0.339140545375525,
                                      0.884983690482392, "x8", 0.467453266437421, 0.781487443696986,
                                      "x9"))
})

# Adding scores to the data used to fail with the polychoric/tetrachoric correlation matrix (jasp-issues#3859)
options <- defaultOptions
options$baseDecompositionOn <- "polychoricTetrachoricCorrelationMatrix"
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")
options$variables.types <- list("scale", "scale", "scale", "ordinal", "scale", "scale")
options$manualNumberOfFactors <- 2
options$scoreSavedToData <- TRUE

test_that("Adding scores to the data works with the polychoric/tetrachoric correlation matrix", {
  for (naAction in c("pairwise", "listwise")) {
    options$naAction <- naAction
    set.seed(1)
    results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = FALSE)
    expect_identical(results[["status"]], "complete", label = paste("naAction =", naAction))
  }
})

# Nominal variables with more than two categories cannot be used with polychoric correlations (jasp-issues#4335)
options <- defaultOptions
options$baseDecompositionOn <- "polychoricTetrachoricCorrelationMatrix"
options$variables <- c("contcor1", "contcor2", "facFive")
options$variables.types <- list("scale", "scale", "nominal")

test_that("Nominal variables with more than two categories give a clear error with the polychoric/tetrachoric correlation matrix", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  tableError <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["error"]][["errorMessage"]]
  expect_match(tableError, "more than two categories")
})

# psych::mixedCor only allows up to 8 categories if there are several ordinal variables (jasp-issues#4005)
options <- defaultOptions
options$baseDecompositionOn <- "polychoricTetrachoricCorrelationMatrix"
options$variables <- c("contcor1", "facFive", "facFifty")
options$variables.types <- list("scale", "ordinal", "ordinal")

test_that("Ordinal variables with more than 8 categories give a clear error with the polychoric/tetrachoric correlation matrix", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  tableError <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["error"]][["errorMessage"]]
  expect_match(tableError, "more than 8 categories")
})

# a single ordinal variable with more than 8 categories (and no nominal variables) is fine for psych::mixedCor (it uses the polyserial correlation)
options$variables <- c("contcor1", "contcor2", "facFifty")
options$variables.types <- list("scale", "scale", "ordinal")

test_that("A single ordinal variable with more than 8 categories can be used with the polychoric/tetrachoric correlation matrix", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  fitTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]]
  expect_null(fitTable[["error"]])
  expect_gt(length(fitTable[["data"]]), 0)
})

# with a nominal variable psych checks the number of categories of the ordinal variables as well
options$variables <- c("contcor1", "facFifty", "facGender")
options$variables.types <- list("scale", "ordinal", "nominal")

test_that("Ordinal variables with more than 8 categories give a clear error if there is also a nominal variable", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  tableError <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["error"]][["errorMessage"]]
  expect_match(tableError, "more than 8 categories")
})

# Imputing missing values when computing the scores (jasp-issues#4621)
options <- defaultOptions
options$variables <- c("contcor1", "contcor2", "debMiss1", "contNormal", "contGamma")
options$variables.types <- list("scale", "scale", "scale", "scale", "scale")
options$scoreSavedToData <- TRUE
options$naAction <- "pairwise"

test_that("Imputing missing values for the scores works", {
  for (imputation in c("none", "mean", "median")) {
    options$scoreSavedToDataImputation <- imputation
    set.seed(1)
    results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, view = FALSE, makeTests = FALSE)
    expect_identical(results[["status"]], "complete", label = paste("imputation =", imputation))
  }
})

# helper for the tests of the imputation of the scores: the scores of the cases without missing values must not change
expectImputationKeepsScores <- function(model, data, baseDecompositionOn, label) {
  scores <- function(imputation, naAction = "pairwise") {
    jaspFactor:::.pcaAndEfaComputeScores(model, data, list(scoreSavedToDataImputation = imputation, naAction = naAction,
                                                           baseDecompositionOn = baseDecompositionOn))
  }
  incomplete    <- !complete.cases(data)
  noObservation <- rowSums(!is.na(data)) == 0
  without       <- scores("none")

  expect_true(all(is.na(without[incomplete, ])), label = paste(label, "without imputation"))
  expect_identical(scores("mean", naAction = "listwise"), without, label = paste(label, "listwise"))

  for (imputation in c("mean", "median")) {
    imputed <- scores(imputation)
    expect_false(anyNA(imputed[!noObservation, ]), label = paste(label, imputation, "all cases with data get a score"))
    expect_true(all(is.na(imputed[noObservation, ])), label = paste(label, imputation, "case without data"))
    expect_equal(unname(imputed[!incomplete, ]), unname(without[!incomplete, ]), label = paste(label, imputation, "complete cases"))
  }
}

simulateScoreData <- function(ordinal = FALSE) {
  set.seed(1)
  latent   <- matrix(rnorm(200 * 2), ncol = 2)
  loadings <- matrix(c(0.8, 0.7, 0.6, 0, 0, 0,
                       0, 0, 0, 0.7, 0.8, 0.6), nrow = 2, byrow = TRUE)
  data <- as.data.frame(latent %*% loadings %*% diag(c(1, 2, 3, 1, 5, 1)) + matrix(rnorm(200 * 6, sd = 0.6), ncol = 6))
  if (ordinal)
    data[] <- lapply(data, function(x) as.integer(cut(x, quantile(x, 0:5 / 5), include.lowest = TRUE)))
  data[c(3, 10), 1] <- NA
  data[c(10, 40), 4] <- NA
  data[120, ]        <- NA # no observed value at all
  return(data)
}

test_that("The scores of cases without missing values do not change when missing values are imputed (EFA)", {
  data <- simulateScoreData()
  expectImputationKeepsScores(suppressWarnings(psych::fa(data, nfactors = 2, rotate = "oblimin", scores = TRUE)),
                              data, "correlationMatrix", "correlation matrix")
  expectImputationKeepsScores(suppressWarnings(psych::fa(data, nfactors = 2, rotate = "varimax", covar = TRUE, scores = TRUE)),
                              data, "covarianceMatrix", "covariance matrix")

  # psych does not return scores for a model that is based on a correlation matrix, as with the polychoric option
  dataOrdinal <- simulateScoreData(ordinal = TRUE)
  rho <- suppressWarnings(psych::mixedCor(dataOrdinal, p = names(dataOrdinal))[["rho"]])
  expectImputationKeepsScores(suppressWarnings(psych::fa(r = rho, nfactors = 2, rotate = "oblimin", n.obs = nrow(dataOrdinal))),
                              dataOrdinal, "polychoricTetrachoricCorrelationMatrix", "polychoric matrix")
})

test_that("Imputation replaces the missing values by the mean or median of the variable before the scores are computed", {
  data   <- simulateScoreData()
  fit    <- suppressWarnings(psych::fa(data, nfactors = 2, rotate = "oblimin", scores = TRUE))
  z      <- scale(as.matrix(data))

  for (imputation in c("mean", "median")) {
    scores <- jaspFactor:::.pcaAndEfaComputeScores(fit, data, list(scoreSavedToDataImputation = imputation, naAction = "pairwise",
                                                                    baseDecompositionOn = "correlationMatrix"))
    fill <- if (imputation == "mean") mean else median
    dataImputed <- data
    for (i in seq_along(dataImputed)) dataImputed[is.na(dataImputed[[i]]), i] <- fill(data[[i]], na.rm = TRUE)
    zImputed <- sweep(sweep(as.matrix(dataImputed), 2, attr(z, "scaled:center")), 2, attr(z, "scaled:scale"), "/")
    expected <- zImputed %*% fit[["weights"]]
    expected[rowSums(!is.na(data)) == 0, ] <- NA
    expect_equal(unname(scores), unname(expected), label = imputation)
  }
})

# Schmid-Leiman transformation (jasp-issues#4621)
test_that("The Schmid-Leiman result is based on the oblique solution and needs at least three factors", {
  fit <- suppressWarnings(suppressMessages(psych::fa(psych::Thurstone, nfactors = 3, n.obs = 213, rotate = "oblimin")))
  options <- list(manualNumberOfFactors = 3, rotationMethod = "oblique")
  result   <- jaspFactor:::.efaSchmidLeimanResult(fit, options)
  expected <- suppressMessages(psych::schmid(fit))

  expect_equal(result[["solution"]], expected[["sl"]])
  expect_equal(result[["generalLoadings"]], as.numeric(expected[["gloading"]]))
  expect_equal(result[["nFactors"]], 3)
  expect_equal(unname(result[["solution"]]["Sentences", "g"]), 0.709, tolerance = 0.01)
  # general and group factor loadings account for the communalities
  expect_equal(unname(rowSums(result[["solution"]][, c("g", "F1*", "F2*", "F3*")]^2)), unname(result[["solution"]][, "h2"]))

  expect_match(jaspFactor:::.efaSchmidLeimanResult(fit, list(manualNumberOfFactors = 2, rotationMethod = "oblique")), "at least three factors")
  fitOrthogonal <- suppressWarnings(suppressMessages(psych::fa(psych::Thurstone, nfactors = 3, n.obs = 213, rotate = "varimax")))
  expect_match(jaspFactor:::.efaSchmidLeimanResult(fitOrthogonal, list(manualNumberOfFactors = 3, rotationMethod = "orthogonal")), "oblique rotation")
})

test_that("The uniquenesses of the Schmid-Leiman solution are right for a covariance matrix", {
  data <- read.csv(testthat::test_path("holzingerswineford.csv"))[, paste0("x", 1:9)]
  fit  <- suppressWarnings(suppressMessages(psych::fa(data, nfactors = 3, rotate = "oblimin", covar = TRUE)))
  result <- jaspFactor:::.efaSchmidLeimanResult(fit, list(manualNumberOfFactors = 3, rotationMethod = "oblique"))

  # psych::schmid itself gives 1 - h2, which is wrong if the variables do not have a variance of 1
  expect_equal(result[["uniquenesses"]], unname(fit[["uniquenesses"]]))
  expect_equal(unname(result[["solution"]][, "h2"]) + result[["uniquenesses"]], unname(diag(stats::cov(data))), tolerance = 1e-6)
})

test_that("The options of the new tables and plots are optional (for example in options of old JASP files)", {
  modelContainer <- jaspBase::createJaspContainer()
  expect_null(jaspFactor:::.efaSchmidLeimanTables(modelContainer, NULL, list(), TRUE))
  expect_null(jaspFactor:::.efaMahalanobisTable(modelContainer, NULL, list(), TRUE))
  expect_null(jaspFactor:::.efaMahalanobisPlot(modelContainer, NULL, list(), TRUE))
})

options <- defaultOptions
options$variables <- paste0("x", 1:9)
options$variables.types <- as.list(rep("scale", 9))
options$manualNumberOfFactors <- 3
options$rotationMethod <- "oblique"
options$schmidLeimanTransformation <- TRUE

test_that("The Schmid-Leiman tables are filled", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "holzingerswineford.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  container <- results[["results"]][["modelContainer"]][["collection"]]
  expect_length(container[["modelContainer_schmidLeimanTable"]][["data"]], 9)
  expect_length(container[["modelContainer_schmidLeimanGeneralTable"]][["data"]], 3)
  expect_null(container[["modelContainer_schmidLeimanTable"]][["error"]])
})

test_that("The Schmid-Leiman transformation gives a clear error with fewer than three factors", {
  options$manualNumberOfFactors <- 2
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "holzingerswineford.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  tableError <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_schmidLeimanTable"]][["error"]][["errorMessage"]]
  expect_match(tableError, "at least three factors")
})

# Mahalanobis outliers (jasp-issues#4621)
test_that("The Mahalanobis distances are those of psych and the outliers are found", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(60 * 4), ncol = 4))
  data[7, ] <- data[7, ] + 6 # an outlier
  data[12, 2] <- NA          # a case with a missing value is not used
  options <- list(dataType = "raw")
  result <- jaspFactor:::.efaMahalanobisResult(data, options)
  complete <- data[-12, ]

  expect_equal(result[["nComplete"]], 59)
  expect_equal(result[["nTotal"]], 60)
  expect_equal(result[["df"]], 4)
  expect_equal(result[["distances"]][["distance"]], unname(stats::mahalanobis(complete, colMeans(complete), stats::cov(complete))))
  expect_equal(result[["distances"]][["p"]], stats::pchisq(result[["distances"]][["distance"]], df = 4, lower.tail = FALSE))
  expect_identical(result[["distances"]][["case"]], rownames(complete))
  expect_identical(result[["distances"]][["case"]][result[["distances"]][["p"]] < 0.001], "7")

  expect_match(jaspFactor:::.efaMahalanobisResult(data, list(dataType = "varianceCovariance")), "raw data")
  expect_match(jaspFactor:::.efaMahalanobisResult(data[1:3, ], options), "not enough cases")
  data[["V5"]] <- data[["V1"]] * 2
  expect_match(jaspFactor:::.efaMahalanobisResult(data, options), "linearly dependent")
})

test_that("The Mahalanobis Q-Q plot can be built", {
  set.seed(1)
  data <- as.data.frame(matrix(rnorm(60 * 4), ncol = 4))
  data[7, ] <- data[7, ] + 6
  result <- jaspFactor:::.efaMahalanobisResult(data, list(dataType = "raw"))
  plot <- jaspFactor:::.efaMahalanobisQQPlot(result, 0.001)
  expect_s3_class(plot, "ggplot")
  expect_identical(nrow(plot[["data"]]), 60L)
  expect_silent(ggplot2::ggplot_build(plot)) # all layers can be built
})

options <- defaultOptions
options$variables <- paste0("x", 1:9)
options$variables.types <- as.list(rep("scale", 9))
options$mahalanobisOutlier <- TRUE
options$mahalanobisOutlierPlot <- TRUE
options$mahalanobisOutlierPValue <- 0.05

test_that("The Mahalanobis outlier table and plot are made", {
  set.seed(1)
  results <- runAnalysis("exploratoryFactorAnalysis", "holzingerswineford.csv", options, view = FALSE, makeTests = FALSE)
  expect_identical(results[["status"]], "complete")
  container <- results[["results"]][["modelContainer"]][["collection"]]
  expect_null(container[["modelContainer_mahalanobisTable"]][["error"]])
  expect_gt(length(container[["modelContainer_mahalanobisTable"]][["data"]]), 0)
  expect_true(all(vapply(container[["modelContainer_mahalanobisTable"]][["data"]], function(row) row[["p"]] < 0.05, logical(1))))
  expect_null(container[["modelContainer_mahalanobisPlot"]][["error"]])
})
