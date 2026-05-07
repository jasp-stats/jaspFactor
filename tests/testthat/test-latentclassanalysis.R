context("Latent Class Analysis")

defaultOptions <- list(
  indicators                    = list(),
  itemResponseProbabilities     = TRUE,
  itemResponseProbabilitiesPlot = FALSE,
  maxIterations                 = 1000,
  missingValues                 = "include",
  models                        = list(list(numberOfClasses = 2)),
  nrep                          = 1,
  plotHeight                    = 320,
  plotWidth                     = 480,
  rotatePlotLabels              = FALSE,
  seed                          = 1
)

options <- defaultOptions
options$indicators <- c("facGender", "facExperim", "facOutlier")

set.seed(1)
results <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options)

test_that("Fit table matches", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(316.2957, 344.9525, 2, 4, 3.52652, -147.1478, 100))
})

test_that("Class prevalences match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_prevalencesTable"]][["data"]]
  # columns alphabetically: class, modalProportion, proportion, se
  jaspTools::expect_equal_tables(table,
    list("Class 1", 0.5, 0.5, 0.05,
         "Class 2", 0.5, 0.5, 0.05))
})

test_that("Item probabilities for facGender match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.58, 0.42, "Class 1",
         0.42, 0.58, "Class 2"))
})


# with plotting
options2 <- defaultOptions
options2$indicators                    <- c("facGender", "facExperim", "facOutlier")
options2$itemResponseProbabilitiesPlot <- TRUE

set.seed(1)
results2 <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options2)

test_that("Item-response probabilities plot matches", {
  plotName <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_itemProbsPlot"]][["data"]]
  testPlot <- results2[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-response-probabilities")
})


# Multimodel stuff
options3 <- defaultOptions
options3$indicators <- c("facGender", "facExperim", "facOutlier")
options3$models     <- list(list(numberOfClasses = 2), list(numberOfClasses = 3))

set.seed(1)
results <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options3, makeTests = FALSE)

test_that("Model Fit table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(316.295658806363, 344.952530852232, 2, 4, 3.52651977149818, -147.147829403181,
                                      100, 326.532398923641, 370.820292085438, 3, -2, 1.76325988877512,
                                      -146.26619946182, 100))
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 0.5, 0.5, 0.05, "Class 2", 0.5, 0.5, 0.05))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 4.12152819805418e-31, "Class 1", 8.51315024548873e-38, 1, "Class 2"
                                 ))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.58, 0.42, "Class 1", 0.42, 0.58, "Class 2"))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_1"]][["collection"]][["modelContainer_model_1_probsContainer"]][["collection"]][["modelContainer_model_1_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.98, 2.80225057927339e-31, 0.02, 1.3192776187808e-31, "Class 1",
                                      2.86323976223379e-38, 0.98, 5.64991048325494e-38, 0.02, "Class 2"
                                 ))
})

test_that("Class Prevalences table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Class 1", 0.5, 0.5, 0.05, "Class 2", 0.21, 0.161431656343223,
                                      0.0397674512394179, "Class 3", 0.29, 0.338568343656776, 0.0509770345645557
                                 ))
})

test_that("facExperim table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 0, "Class 1", 0, 1, "Class 2", 0, 1, "Class 3"))
})

test_that("facGender table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.58, 0.42, "Class 1", 0.999999998487003, 1.51299744924493e-09,
                                      "Class 2", 0.143452111843801, 0.856547888156199, "Class 3"
                                 ))
})

test_that("facOutlier table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_model_2"]][["collection"]][["modelContainer_model_2_probsContainer"]][["collection"]][["modelContainer_model_2_probsContainer_facOutlier"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.98, 0, 0.02, 0, "Class 1", 0, 0.938054281133443, 0, 0.0619457188665573,
                                      "Class 2", 0, 1, 0, 1.0304301072485e-263, "Class 3"))
})
