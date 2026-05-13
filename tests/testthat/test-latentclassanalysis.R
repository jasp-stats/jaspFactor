context("Latent Class Analysis")

defaultOptions <- list(
  indicators                    = list(),
  itemResponseProbabilities     = TRUE,
  itemResponseProbabilitiesPlot = FALSE,
  maxIterations                 = 1000,
  missingValues                 = "include",
  numberOfClasses               = 2,
  nrep                          = 1,
  plotHeight                    = 320,
  plotWidth                     = 480,
  rotatePlotLabels              = FALSE,
  saveClassProbabilities        = FALSE,
  saveClassProbabilitiesPrefix  = "classProb",
  saveClassification            = FALSE,
  saveClassificationColumn      = "classAssignment",
  saveForClasses                = 2,
  seed                          = 1,
  showLevelsLegend              = FALSE
)

options <- defaultOptions
options$indicators <- c("facGender", "facExperim", "facOutlier")

set.seed(1)
results <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options, makeTests = FALSE)

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(445.496130991918, 458.521981921859, 1, 10, 144.726991957046, -217.748065495959,
                                      100, 316.295658806363, 344.952530852232, 2, 4, 3.52651977149818,
                                      -147.147829403181, 100))
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 1, 1, ""))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.5, 0.5, "Class 1"))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.5, 0.5, "Class 1"))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.49, 0.49, 0.01, 0.01, "Class 1"))
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 0.5, 0.5, 0.05, "Class 2", 0.5, 0.5, 0.05))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 4.12152819805418e-31, "Class 1", 8.51315024548873e-38, 1, "Class 2"
                                 ))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.58, 0.42, "Class 1", 0.42, 0.58, "Class 2"))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.98, 2.80225057927339e-31, 0.02, 1.3192776187808e-31, "Class 1",
                                      2.86323976223379e-38, 0.98, 5.64991048325494e-38, 0.02, "Class 2"
                                 ))
})

# with plotting
options2 <- defaultOptions
options2$indicators                    <- c("facGender", "facExperim", "facOutlier")
options2$itemResponseProbabilitiesPlot <- TRUE

set.seed(1)
results <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options2, makeTests = FALSE)

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(445.496130991918, 458.521981921859, 1, 10, 144.726991957046, -217.748065495959,
                                      100, 316.295658806363, 344.952530852232, 2, 4, 3.52651977149818,
                                      -147.147829403181, 100))
})

test_that("Item-Response Probabilities plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_itemProbsPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-response-probabilities1")
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 1, 1, ""))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.5, 0.5, "Class 1"))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.5, 0.5, "Class 1"))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.49, 0.49, 0.01, 0.01, "Class 1"))
})

test_that("Item-Response Probabilities plot matches", {
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_itemProbsPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-response-probabilities2")
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 0.5, 0.5, 0.05, "Class 2", 0.5, 0.5, 0.05))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 4.12152819805418e-31, "Class 1", 8.51315024548873e-38, 1, "Class 2"
                                 ))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.58, 0.42, "Class 1", 0.42, 0.58, "Class 2"))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.98, 2.80225057927339e-31, 0.02, 1.3192776187808e-31, "Class 1",
                                      2.86323976223379e-38, 0.98, 5.64991048325494e-38, 0.02, "Class 2"
                                 ))
})
