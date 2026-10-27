context("Number of Factors/Components")

# NOTE on expected values:
# The parallel analysis tests were moved here from the pre-split EFA/PCA tests.
# There, fa.parallel ran multiple times per analysis (different RNG path) with
# the session seed; here it runs once with setSeed = TRUE. Real-data eigenvalues
# (val1) are unchanged; simulated-data values (val2) and the parallel analysis
# suggestion were regenerated.

defaultOptions <- list(
  variables = list(),
  dataType = "raw",
  sampleSize = 200,
  eigenvalueMethod = "principalComponentBased",
  parallelAnalysisIterations = 20,
  eigenvalueThreshold = 1,
  correlationMatrix = "correlationMatrix",
  parallelAnalysisTable = TRUE,
  mapTable = TRUE,
  screePlot = TRUE,
  screePlotParallelAnalysisResults = TRUE,
  naAction = "pairwise",
  plotWidth = 480,
  plotHeight = 320,
  setSeed = TRUE,
  seed = 1
)


# PC-based parallel analysis on Pearson correlations
options <- defaultOptions
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Summary table results match (PC-based)", {
  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Parallel analysis (PC-based)", 2,
                                      "Eigenvalue criterion (above 1)", 3,
                                      "Velicer's MAP", 0))
})

test_that("MAP table results match", {
  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_mapTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("0*", 0.04314047779, "1", 0.05852907536, "2", 0.11721132093,
                                      "3", 0.2207663532, "4", 0.46692540953, "5", 1))
})

test_that("Parallel Analysis table results match (PC-based)", {
  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_parallelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1*", 1.7795916550878, 1.36583483001393, "Component 2*",
                                      1.28644706023115, 1.1795441336838, "Component 3", 1.08333785331839,
                                      1.04563430924183, "Component 4", 0.848949206589453, 0.92484523771976,
                                      "Component 5", 0.696170865182367, 0.805435846758642, "Component 6",
                                      0.305503359590833, 0.678705642582045))
})

test_that("Scree plot matches (PC-based)", {
  plotName <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_screePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scree-plot-pc")
})


# FA-based parallel analysis
options <- defaultOptions
options$eigenvalueMethod <- "factorBased"
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Summary and Parallel Analysis tables use FA basis", {
  summaryData <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_summaryTable"]][["data"]]
  testthat::expect_length(summaryData, 3L)
  testthat::expect_true(grepl("FA-based", summaryData[[1]][["method"]]))

  parallelData <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_parallelTable"]][["data"]]
  testthat::expect_length(parallelData, 6L)
  testthat::expect_true(all(grepl("^Factor", sapply(parallelData, `[[`, "col"))))
})

faEigenvalues <- sapply(results[["results"]][["nofContainer"]][["collection"]][["nofContainer_parallelTable"]][["data"]],
                        `[[`, "val1")


# FA-based, simulated data overlay hidden: real data series must stay on the factor eigenvalues
options <- defaultOptions
options$eigenvalueMethod <- "factorBased"
options$screePlotParallelAnalysisResults <- FALSE
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Scree plot keeps factor eigenvalues when parallel analysis overlay is hidden (FA-based)", {
  plotName <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_screePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  testthat::expect_identical(unique(as.character(testPlot[["data"]][["type"]])), "Data")
  testthat::expect_equal(testPlot[["data"]][["ev"]], faEigenvalues, tolerance = 1e-6)
  testthat::expect_identical(testPlot[["labels"]][["x"]], "Factor")
})


# FA-based on two uncorrelated variables: fa.parallel suggests 0 factors,
# the summary must report 0 and no row may be starred
options <- defaultOptions
options$eigenvalueMethod <- "factorBased"
options$variables <- c("contNormal", "contGamma")

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Zero parallel-analysis suggestion is not forced to one", {
  summaryData <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_summaryTable"]][["data"]]
  testthat::expect_identical(summaryData[[1]][["method"]], "Parallel analysis (FA-based)")
  testthat::expect_equal(summaryData[[1]][["n"]], 0)

  parallelData <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_parallelTable"]][["data"]]
  testthat::expect_false(any(grepl("*", sapply(parallelData, `[[`, "col"), fixed = TRUE)))
})


# PC-based parallel analysis on polychoric/tetrachoric correlations
options <- defaultOptions
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")
options$variables.types <- list("scale", "scale", "scale", "ordinal", "scale", "scale")
options$correlationMatrix <- "polyTetrachoricCorrelationMatrix"

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Parallel Analysis table results match with poly cor", {
  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_parallelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1*", 1.78311572348898, 1.33849952828362, "Component 2*",
                                      1.28924116893078, 1.19495601423762, "Component 3", 1.08833059622023,
                                      1.03657350841346, "Component 4", 0.845932695389084, 0.919703170460585,
                                      "Component 5", 0.688011322780564, 0.817254742438021, "Component 6",
                                      0.305368493190363, 0.693013036166699))
})


# eigenvalue threshold affects only the summary table
options <- defaultOptions
options$variables <- c("contcor1", "contcor2", "facFifty", "facFive", "contNormal", "debMiss1")
options$eigenvalueThreshold <- 0.8

results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options, makeTests = F)

test_that("Eigenvalue criterion respects the threshold", {
  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Parallel analysis (PC-based)", 2,
                                      "Eigenvalue criterion (above 0.8)", 4,
                                      "Velicer's MAP", 0))
})


# Holzinger-Swineford data (9 variables, 301 cases): MAP suggests 2 components,
# all values equal the reference from EFA.dimensions::MAP (v0.1.9.2)
options <- defaultOptions
options$variables <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")

results <- jaspTools::runAnalysis("numberOfFactors", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("MAP results match EFA.dimensions (Holzinger-Swineford)", {
  summaryData <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_summaryTable"]][["data"]]
  testthat::expect_identical(summaryData[[3]][["method"]], "Velicer's MAP")
  testthat::expect_equal(summaryData[[3]][["n"]], 2)

  table <- results[["results"]][["nofContainer"]][["collection"]][["nofContainer_mapTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("0", 0.10188449563, "1", 0.0802261369, "2*", 0.06341522893,
                                      "3", 0.06822706134, "4", 0.12005229672, "5", 0.206652421,
                                      "6", 0.34253617281, "7", 0.44266426238, "8", 1))
})


# MAP values must equal psych::vss for 1 or more components; 0 components is added on top
test_that("MAP values match psych::vss", {
  corMatrix <- as.matrix(psych::Thurstone)
  mapValues <- jaspFactor:::.nofMapValues(corMatrix)
  vssMap    <- suppressWarnings(suppressMessages(psych::vss(corMatrix, n = 8, n.obs = 213, plot = FALSE)$map))

  testthat::expect_equal(mapValues[-1], vssMap, tolerance = 1e-8)
  testthat::expect_equal(jaspFactor:::.nofMapSuggested(mapValues), 3)
})


test_that("Analysis does not crash without variables", {
  options <- defaultOptions
  results <- jaspTools::runAnalysis("numberOfFactors", "test.csv", options)
  testthat::expect_identical(results[["status"]], "complete")
})
