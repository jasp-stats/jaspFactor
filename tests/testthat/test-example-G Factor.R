context("Example: G Factor")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("exploratoryFactorAnalysis (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "G Factor.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("exploratoryFactorAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Factor 1", 0.466283987164474, 0.466284032177272, 0.466283987164474,
     0.466284032177272, 3.26398791015131, 3.2639882252409))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_fitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-26.0735819236659, 0.941844735980518, 0.0994858437911664, "0 - 0.247",
     0.0924937322585127, 0.908493260927005))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(17.8233370993422, 14, "Model", 0.214945809355892))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.964744613253794, 0.0692678311977876, "jaspColumn4", 0.905697406226147,
     0.179712208355229, "jaspColumn5", 0.850593590274152, 0.276490544184528,
     "jaspColumn7", 0.728677939153591, 0.469028460990875, "jaspColumn6",
     0.4314361689816, 0.81386283209448, "jaspColumn1", 0.218273887553604,
     0.952356510012237, "jaspColumn2", 0.157182368561013, 0.97529370301355,
     "jaspColumn3"))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_path"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_path-diagram")

  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_scree-plot")

})

test_that("principalComponentAnalysis (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "G Factor.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("principalComponentAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Component 1", 0.507309798046495, 0.507309798046495, 3.55116858632547,
     3.55116858632546, 0.507309798046495, 0.507309798046495))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(20.4153154450051, 14, "Model", 0.117581780629009))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.940367460295704, 0.115709039617007, "jaspColumn4", 0.907297755977426,
     0.176810781998327, "jaspColumn5", 0.881854666863415, 0.222332346531216,
     "jaspColumn7", 0.805259591154695, 0.351556990853373, "jaspColumn6",
     0.531221072557435, 0.717804172070928, "jaspColumn1", 0.292003528877688,
     0.914733939122977, "jaspColumn2", 0.223865710905652, 0.949884143480707,
     "jaspColumn3"))

  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_scree-plot")

})


