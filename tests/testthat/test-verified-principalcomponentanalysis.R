context("Principal Component Analysis -- Verification project")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - slider


## Testing Questionnaire data 

# https://jasp-stats.github.io/jasp-verification-project/factor.html
options <- jaspTools::analysisOptions("PrincipalComponentAnalysis")
options$PCPrefix <- ""
options$loadingsThreshold <- 0.4
options$orthogonalSelector <- "varimax"
options$variables <- c(paste("Question", 1:9, sep="_0"), paste("Question", 10:23, sep="_"))
options$factorMethod <- "parallelAnalysis"
options$rotationMethod <- "orthogonal"
options$basedOn <- "correlation"
options$missingValues <- "pairwise"
options$incl_screePlot <- TRUE

set.seed(1)
results <- jaspTools::runAnalysis("PrincipalComponentAnalysis", "PCA.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/factor.html
test_that("Chi-squared Test table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(
    "test"=resultsTable,
    "ref"=list(2634.37444035402, 167, "Model", 0)
  )
})


# https://jasp-stats.github.io/jasp-verification-project/factor.html
test_that("Component Loadings table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
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
test_that("Component Characteristics table results match R, SPSS, SAS, MiniTab", {
  resultsTable <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  jaspTools::expect_equal_tables(
    "test"=resultsTable,
    "ref"=list("PC1", 0.316958567983434, 7.29004706361899, 0.316958567983434,
               "PC2", 0.392559817846783, 1.73882874685703, 0.0756012498633492,
               "PC3", 0.449809884276163, 1.31675152787573, 0.0572500664293797,
               "PC4", 0.503166325737664, 1.22719815361453, 0.0533564414615014
    )
  )
})

# # https://jasp-stats.github.io/jasp-verification-project/factor.html
# test_that("Scree plot matches R, SPSS, SAS, MiniTab", {
#   skip("Scree plot check does not work because some data is simulated (non-deterministic).")
#   plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   jaspTools::expect_equal_plots(testPlot, "scree-plot")
# })

options <- jaspTools::analysisOptions("PrincipalComponentAnalysis")
options$variables <- list("contNormal", "contGamma", "debCollin1", "contcor1", "facFifty")
options$eigenValuesBox <- 0.95
options$orthogonalSelector <- "varimax"
options$incl_pathDiagram <- TRUE
options$incl_screePlot <- TRUE
options$factorMethod <- "eigenValues"
set.seed(1)
results <- jaspTools::runAnalysis("PrincipalComponentAnalysis", "test.csv", options)


test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goftab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(56.1723464768203, 1, "Model", 6.63887442169672e-14))
})

test_that("Component Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.709068975944499, -0.055882219913321, 0.494098364850579, "contNormal",
                           -0.198414056307147, -0.730807163622534, 0.426552751857732, "contGamma",
                           -0.154267640888903, 0.766942636295035, 0.388000487607395, "debCollin1",
                           0.613519408318389, 0.258607271436745, 0.556716214776696, "contcor1",
                           -0.560112933829558, 0.0519207989938901, 0.683577731988681, "facFifty"
                      ))
})

test_that("Component Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list("PC1", 0.269220893232411, 1.34610446616205, 0.269220893232411,
                           "PC2", 0.490210889783784, 1.10494998275686, 0.220989996551372
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
