context("Exploratory Factor Analysis")

# does not test
# - error handling
# - orthogonal rotation
# - Eigen values above / manual
# - contents of screeplot (set.seed does not work)

options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
options$factorMethod <- "manual"
options$fitmethod <- "minres"
options$highlightText <- 0.4
options$incl_correlations <- TRUE
options$incl_fitIndices <- TRUE
options$incl_pathDiagram <- TRUE
options$incl_screePlot <- TRUE
options$incl_structure <- TRUE
options$numberOfFactors <- 2
options$obliqueSelector <- "geominQ"
options$rotationMethod <- "oblique"
options$factorLoadingsSort <- "sortByVariables"
options$variables <- list("contWide", "contcor1", "contcor2", "facFifty", "contExpon",
                          "debCollin1", "debEqual1")
set.seed(1)
results <- jaspTools::runAnalysis("ExploratoryFactorAnalysis", "debug.csv", options)



test_that("Factor Correlations table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_cortab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, -0.0736228, "Factor 1", -0.0736228, 1, "Factor 2"))
})

test_that("Factor Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.211560139237577, 0.21520386846338, 0.211560139237577,
                                      0.21520386846338, 1.48092097466304, 1.50642707924366, "Factor 2",
                                      0.366100386048402, 0.366966592875575, 0.154540246810825, 0.151762724412195,
                                      1.08178172767577, 1.06233907088537))
})

test_that("Additional fit indices table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fittab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-32.7898349546892, 0, "0 - 0.065", 1.20127892716016))
})

test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(4.05152653321549, 8, "Model", 0.85244487039262))
})

test_that("Factor Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("", "", 0.951432334368898, "contWide", 0.654092561077089, "",
                           0.57413710933047, "contcor1", 1.00020594814694, "", -0.00255707470903843,
                           "contcor2", "", "", 0.953108905280117, "facFifty", "", 0.997800455330077,
                           0.00428892032601724, "contExpon", "", "", 0.998135387367175,
                           "debCollin1", "", "", 0.958751715702742, "debEqual1"))
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
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_strtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("", "", "contWide", 0.651914307711847, "", "contcor1", 1.00118914256335,
                           "", "contcor2", "", "", "facFifty", "", 0.997852981864278, "contExpon",
                           "", "", "debCollin1", "", "", "debEqual1"))
})

test_that("Missing values works", {
  options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- list("contNormal", "contGamma", "contcor1", "debMiss30")
  options$incl_correlations <- TRUE

  options$missingValues <- "pairwise"
  results <- jaspTools::runAnalysis("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(table, list("Model", 1.42781053334818, 2L, 0.489727939944839), label = "pairwise")

  options$missingValues <- "listwise"
  results <- jaspTools::runAnalysis("ExploratoryFactorAnalysis", "test.csv", options)
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(table, list("Model", 0.491396758561133, 2L, 0.782158104440787), label = "listwise")
})



options$factorLoadingsSort <- "sortByVariables"


test_that("factorLoadingsSort sort the factor loadings table", {

  options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
  options$orthogonalSelector <- "varimax"
  options$highlightText <- 0.2
  options$variables <- paste0("x", 1:9)

  reference <- list(
    sortByFactorSize = list(
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

  for (factorLoadingsSort in c("sortByFactorSize", "sortByVariables")) {
    options$factorLoadingsSort <- factorLoadingsSort

    set.seed(123)
    results <- runAnalysis("ExploratoryFactorAnalysis", "holzingerswineford.csv", options)

    table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
    jaspTools::expect_equal_tables(table, reference[[factorLoadingsSort]], label = sprintf("factorLoadingsSort = %s", factorLoadingsSort))
  }

})

test_that("Estimation options do not crash", {
  options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
  options$variables <- paste0("Q0", 1:9)

  for(fitmethod in c("minres", "ml", "pa", "ols", "wls", "gls", "minchi", "minrank")) {
    options$fitmethod <- fitmethod
    results <- runAnalysis("ExploratoryFactorAnalysis", "Fear of Statistics.csv", options)
    testthat::expect(is.null(results[["results"]][["error"]]),
                     sprintf("Estimation with method '%s' crashes", fitmethod))
  }
})


options <- jaspTools::analysisOptions("ExploratoryFactorAnalysis")
options$factorMethod <- "parallelAnalysis"
options$parallelMethod <- "pc"
options$fitmethod <- "minres"
options$highlightText <- 0.1
options$incl_correlations <- TRUE
options$incl_screePlot <- TRUE
options$orthogonalSelector <- "none"
options$rotationMethod <- "orthogonal"
options$variables <- paste0("x", 1:9)
set.seed(1)
results <- runAnalysis("ExploratoryFactorAnalysis", "holzingerswineford.csv", options)

test_that("Factor Characteristics table results match with parallel analysis based on PCs", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Factor 1", 0.314163998816933, 0.314163998816933, 2.82747598935239,
                                      "Factor 2", 0.449126711506194, 0.134962712689261, 1.21466441420335,
                                      "Factor 3", 0.539738017727465, 0.0906113062212709, 0.815501755991438
                                 ))
})

test_that("Chi-squared Test table results match with parallel analysis based on PCs", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(22.5550491736693, 12, "Model", 0.0317499059313278))
})

test_that("Factor Loadings table results match with parallel analysis based on PCs", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
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
