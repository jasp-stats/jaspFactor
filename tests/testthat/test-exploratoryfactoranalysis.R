context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

defaultOptions <- list(
  variables = list(),
  sampleSize = 200,
  eigenValuesAbove = 1,
  manualNumberOfFactors = 1,
  factoringMethod = "minimumResidual",
  orthogonalSelector = "none",
  obliqueSelector = "promax",
  loadingsDisplayLimit = 0,
  factorStructure = FALSE,
  factorCorrelations = FALSE,
  fitIndices = FALSE,
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
  factorCountMethod = "parallelAnalysis",
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
options$factorCountMethod <- "manual"
options$factoringMethod <- "minimumResidual"
options$loadingsDisplayLimit <- 0.4
options$factorCorrelations <- TRUE
options$fitIndices <- TRUE
options$pathDiagram <- TRUE
options$screePlot <- TRUE
options$factorStructure <- TRUE
options$residualMatrix <- TRUE
options$manualNumberOfFactors <- 2
options$obliqueSelector <- "geominQ"
options$rotationMethod <- "oblique"
options$loadingsOrder <- "sortByVariables"
options$variables <- list("contWide", "contcor1", "contcor2", "facFifty", "contExpon",
                          "debCollin1", "debEqual1")
set.seed(1)
results <- jaspTools::runAnalysis("exploratoryFactorAnalysis", "test.csv", options)


test_that("Factor Correlations table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_correlationTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, -0.0736228, "Factor 1", -0.0736228, 1, "Factor 2"))
})

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.211560139237577, 0.21520386846338, 1.76545396982125,
                                      0.211560139237577, 0.21520386846338, 1.48092097466304, 1.50642707924366,
                                      "Factor 2", 0.366100386048402, 0.366966592875575, 1.31015305219849,
                                      0.154540246810825, 0.151762724412195, 1.08178172767577, 1.06233907088537
                                 ))
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

test_that("Scree plot matches", {
  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scree-plot")
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
  options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
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



options$loadingsOrder <- "sortByVariables"


test_that("loadingsOrder sort the factor loadings table", {

  options <- defaultOptions
  options$orthogonalSelector <- "varimax"
  options$loadingsDisplayLimit <- 0.2
  options$variables <- paste0("x", 1:9)

  reference <- list(
    sortBySize = list(
      0.859092745287366, "", "", 0.246269423771584, "x5", 0.832032514222841,
      "", "", 0.27206434823954, "x4", 0.798985079015342, 0.213747389751454,
      "", 0.308644745749111, "x6", 0.279356098904291, 0.612821903382293,
      "", 0.523245992219826, "x1", "", 0.659769507411633, "", 0.546549851738645,
      "x3", "", 0.493809804203504, "", 0.744772821204617, "x2", "",
      0.414642352102081, 0.521227164426046, 0.539538616482869, "x9",
      "", "", 0.709321543667037, 0.481445404925259, "x7", "", "",
      0.698776373366381, 0.479827748357628, "x8"
    ),
    sortByVariables = list(
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

  for (loadingsOrder in c("sortBySize", "sortByVariables")) {
    options$loadingsOrder <- loadingsOrder

    set.seed(123)
    results <- runAnalysis("exploratoryFactorAnalysis", "holzingerswineford.csv", options)

    table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
    jaspTools::expect_equal_tables(table, reference[[loadingsOrder]], label = sprintf("loadingsOrder = %s", loadingsOrder))
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


options <- defaultOptions
options$factorCountMethod <- "parallelAnalysis"
options$parallelAnalysisMethod <- "principalComponentBased"
options$factoringMethod <- "minimumResidual"
options$loadingsDisplayLimit <- 0.1
options$factorCorrelations <- TRUE
options$screePlot <- TRUE
options$orthogonalSelector <- "none"
options$rotationMethod <- "orthogonal"
options$variables <- paste0("x", 1:9)
set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options)


test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.314163998816933, 3.21634418143771, 0.314163998816933,
                                      2.8274759893524, "Factor 2", 0.449126711506194, 1.63871322152606,
                                      0.134962712689261, 1.21466441420335, "Factor 3", 0.539738017727465,
                                      1.36515934778625, 0.0906113062212709, 0.815501755991438))
})

test_that("Chi-squared Test table results match with parallel analysis based on PCs", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5550491736693, 12, "Model", 0.0317499059313278))
})

test_that("Factor Loadings table results match with parallel analysis based on PCs", {

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

test_that("Scree plot matches", {
  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scree-plot")
})


options <- defaultOptions
options$factorCountMethod <- "parallelAnalysis"
options$parallelAnalysisMethod <- "principalComponentBased"
options$loadingsDisplayLimit <- 0.1
options$analysisBasedOn <- "polyTetrachoricCorrelationMatrix"
options$mardiaTest <- TRUE
options$parallelAnalysisTable <- TRUE
options$rotationMethod <- "oblique"
options$factoringMethod <- "minimumResidual"
options$variables <- list("contcor1", "contcor2", "facFifty", "facFive","contNormal", "debMiss1")

set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = F)

test_that("Factor Characteristics table results match with poly cor", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.237444621938795, 0.238183139137491, 1.78311572348898,
                                      0.237444621938795, 0.238183139137491, 1.42466773163277, 1.42909883482494,
                                      "Factor 2", 0.411139518227707, 0.411182023092321, 1.28924116893078,
                                      0.173694896288912, 0.17299888395483, 1.04216937773347, 1.03799330372898
                                 ))
})

test_that("Mardia's Test of Multivariate Normality table results match with poly cor", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_mardiaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.0201435883819, 56, 0.706100154506541, 49.8323692083014, "Skewness",
                                      3.0201435883819, 56, 0.635010590136702, 51.78632377773, "Small Sample Skewness",
                                      44.9365639232773, 0.119834882997249, -1.55546702109016, "Kurtosis"
                                 ))
})

test_that("Parallel Analysis table results match with poly cor", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parallelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1*", 1.78311572348898, 1.34293486623467, "Factor 2*",
                                      1.28924116893078, 1.18287911279811, "Factor 3*", 1.08833059622023,
                                      1.05066617046688, "Factor 4", 0.845932695389084, 0.925279735884113,
                                      "Factor 5", 0.688011322780564, 0.809825771393728, "Factor 6",
                                      0.305368493190363, 0.688414343222493))
})



options <- defaultOptions
options$factorCountMethod <- "parallelAnalysis"
options$parallelAnalysisMethod <- "principalComponentBased"
options$parallelAnalysisTable <- TRUE
options$rotationMethod <- "oblique"
options$variables <- list("contcor1", "contcor2", "facFifty", "facFive","contNormal", "debMiss1")

options("mc.cores" = 1L)
set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", "test.csv", options, makeTests = F)

test_that("Parallel Analysis table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parallelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1*", 1.7795916550878, 1.33053625507377, "Factor 2*", 1.28644706023115,
                                      1.17402737320915, "Factor 3*", 1.08333785331839, 1.0367445878489,
                                      "Factor 4", 0.848949206589453, 0.923477592629848, "Factor 5",
                                      0.696170865182367, 0.837720518530386, "Factor 6", 0.305503359590833,
                                      0.697493672707948))
})


# variance covariance matrix input
dt <- read.csv(testthat::test_path("holzingerswineford.csv"))
cdt <- as.data.frame(cov(dt[, 7:15]))
options <- list(
  addScores = FALSE,
  addedScoresPrefix = "FA",
  analysisBasedOn = "correlationMatrix",
  bartlettTest = FALSE,
  dataType = "varianceCovariance",
  eigenValuesAbove = 1,
  factorCorrelations = FALSE,
  factorCountMethod = "manual",
  loadingsOrder = "sortByVariables",
  factorStructure = FALSE,
  factoringMethod = "minimumResidual",
  fitIndices = FALSE,
  kaiserMeyerOlkinTest = FALSE,
  loadingsDisplayLimit = 0.1,
  manualNumberOfFactors = 1,
  mardiaTest = FALSE,
  naAction = "pairwise",
  obliqueSelector = "promax",
  orthogonalSelector = "none",
  parallelAnalysisMethod = "principalComponentBased",
  parallelAnalysisSeed = 1234,
  parallelAnalysisTable = FALSE,
  parallelAnalysisTableMethod = "principalComponentBased",
  pathDiagram = FALSE,
  plotHeight = 320,
  plotWidth = 480,
  residualMatrix = FALSE,
  rotationMethod = "orthogonal",
  sampleSize = 200,
  screePlot = FALSE,
  screePlotParallelAnalysisResults = TRUE,
  variables = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
)

set.seed(1)
results <- runAnalysis("exploratoryFactorAnalysis", cdt, options, makeTests = F)

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.292468690109419, 3.21634418143771, 0.292468690109419,
                                      2.63221821098477))
})

test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(235.832782509408, 27, "Model", 3.1571356942318e-35))
})

test_that("Factor Loadings table results match", {
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
