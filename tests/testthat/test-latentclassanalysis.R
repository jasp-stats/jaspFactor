context("Latent Class Analysis")

defaultOptions <- list(
  indicators                    = list(),
  itemResponseProbabilities     = TRUE,
  itemResponseProbabilitiesPlot = FALSE,
  maxIterations                 = 1000,
  missingValues                 = "listwise",
  nrep                          = 1,
  numberOfClasses               = 2,
  plotHeight                    = 320,
  plotWidth                     = 480,
  rotatePlotLabels              = FALSE,
  seed                          = 1
)

options <- defaultOptions
options$indicators      <- c("facGender", "facExperim", "facOutlier")
options$numberOfClasses <- 2

set.seed(1)
results <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options)

test_that("Fit table matches", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(316.2957, 344.9525, 4, 3.52652, -147.1478, 100))
})

test_that("Class prevalences match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_prevalencesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Class 1", 0.5, 0.05,
         "Class 2", 0.5, 0.05))
})

test_that("Item probabilities for facGender match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_probsContainer"]][["collection"]][["modelContainer_probsContainer_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.58, 0.42, "Class 1",
         0.42, 0.58, "Class 2"))
})

options2 <- defaultOptions
options2$indicators                    <- c("facGender", "facExperim", "facOutlier")
options2$numberOfClasses               <- 2
options2$itemResponseProbabilitiesPlot <- TRUE

set.seed(1)
results2 <- jaspTools::runAnalysis("latentClassAnalysis", "test.csv", options2)

test_that("Item-response probabilities plot matches", {
  plotName <- results2[["results"]][["modelContainer"]][["collection"]][["modelContainer_itemProbsPlot"]][["data"]]
  testPlot <- results2[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "item-response-probabilities")
})
