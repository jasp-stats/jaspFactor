context("Principal Component Analysis")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - slider


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

rotationOptions <- list(
  "orthogonal" = c("none", "varimax", "quartimax", "bentlerT", "equamax", "geominT"),
  "oblique"    = c("promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster", "geominQ")
)

options <- analysisOptions("PrincipalComponentAnalysis")
options$factorMethod <- "eigenValues"
options$variables <- c("contNormal", "contGamma", "contExpon", "contWide", "contNarrow", "contOutlier", "contcor1", "contcor2", "debMiss1", "debCollin1")
jaspTableToRTable <- function(x) do.call(rbind, lapply(x, do.call, what = cbind.data.frame))

allResults <- list(orthogonal = list(none = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
varimax = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
quartimax = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
bentlerT = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
equamax = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
geominT = data.frame(
  comp = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
)),
oblique = list(promax = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
oblimin = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
simplimax = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
bentlerQ = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
biquartimin = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
cluster = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
geominQ = data.frame(
  comp = c("RC1", "RC2", "RC3", "RC4", "RC5"),
  cump = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigv = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  prop = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
)))

# uncomment the commented code to update the results
# allResults <- list("orthogonal" = list(), "oblique" = list())

test_that("rotation methods match", {
  for (rotationMethod in names(rotationOptions)) {

    options[["rotationMethod"]] <- rotationMethod
    for (rotation in rotationOptions[[rotationMethod]]) {

      if (rotationMethod == "orthogonal") {
        options[["orthogonalSelector"]] <- rotation
      } else {
        options[["obliqueSelector"]] <- rotation
      }

      set.seed(1)
      results <- runAnalysis("PrincipalComponentAnalysis", "test.csv", options, view = FALSE)
      tb <- jaspTableToRTable(results$results$modelContainer$collection$modelContainer_eigtab$data)

      testthat::expect_equal(object = tb, expected = allResults[[rotationMethod]][[rotation]], label = paste(rotationMethod, "-", rotation))
      # allResults[[rotationMethod]][[rotation]] <- tb
    }
  }
})

# # more readable than dput
# dputAllResults <- function(x) {
#   addCommas <- function(x) {
#     x[1:(length(x) - 1)] <- paste0(x[1:(length(x) - 1)], ",")
#     x
#   }
#   dputDf <- function(df) {
#     tmp <- sapply(df, function(x) paste(capture.output(dput(x)), collapse = ""))
#     columns <- paste(sprintf("  %s = %s", names(df), addCommas(tmp)), collapse = "\n")
#     sprintf("data.frame(\n%s\n)", columns)
#   }
#
#   tmps <- lapply(seq_along(allResults), function(i) {
#     tmp <- paste(names(allResults[[i]]), "=", sapply(allResults[[i]], dputDf))
#     tmp <- addCommas(tmp)
#     tmp <- paste(tmp, collapse = "\n")
#   })
#
#   string <- sprintf("list(%s = list(%s),\n%s = list(%s))", names(allResults[1]), tmps[[1]], names(allResults[2]), tmps[[2]])
#   cat(string)
#   invisible(string)
#
# }
# dputAllResults(allResults)
