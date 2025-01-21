context("Principal Component Analysis")

# does not test
# - error handling
# - slider

defaultOptions <- list(
  variables = list(),
  sampleSize = 200,
  eigenValuesAbove = 1,
  manualNumberOfComponents = 1,
  orthogonalSelector = "none",
  obliqueSelector = "promax",
  loadingsDisplayLimit = 0,
  componentCorrelations = FALSE,
  residualMatrix = FALSE,
  parallelAnalysisTable = FALSE,
  pathDiagram = FALSE,
  screePlot = FALSE,
  screePlotParallelAnalysisResults = TRUE,
  kaiserMeyerOlkinTest = FALSE,
  bartlettTest = FALSE,
  mardiaTest = FALSE,
  addScores = FALSE,
  antiImageCorrelationMatrix = FALSE,
  addedScoresPrefix = "",
  dataType = "raw",
  componentCountMethod = "parallelAnalysis",
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
options$variables <- list("contNormal", "contGamma", "debCollin1", "contcor1", "facFifty")
options$eigenValuesAbove <- 0.95
options$orthogonalSelector <- "varimax"
options$pathDiagram <- TRUE
options$screePlot <- TRUE
options$residualMatrix <- TRUE
options$componentCountMethod <- "eigenValues"

set.seed(1)
results <- jaspTools::runAnalysis("principalComponentAnalysis", "test.csv", options)


test_that("Chi-squared Test table results match", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(56.1723464768203, 1, "Model", 6.63887442169672e-14))
})

test_that("Component Loadings table results match", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.709068975944499, -0.055882219913321, 0.494098364850579, "contNormal",
                           -0.198414056307147, -0.730807163622534, 0.426552751857732, "contGamma",
                           -0.154267640888903, 0.766942636295035, 0.388000487607395, "debCollin1",
                           0.613519408318389, 0.258607271436745, 0.556716214776696, "contcor1",
                           -0.560112933829558, 0.0519207989938901, 0.683577731988681, "facFifty"
                      ))
})

test_that("Component Characteristics table results match with varimax rotation", {

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1", 0.251215603687833, 0.269220893232411, 1.25607801843916,
                                      1.34610446616205, 0.251215603687833, 0.269220893232411, "Component 2",
                                      0.490210889783784, 0.490210889783784, 1.19497643047975, 1.10494998275686,
                                      0.238995286095951, 0.220989996551372))
})

test_that("Residual Matrix table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_residualTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", 0.494098364850579, 0.0406497391363535, 0.142790676592631,
                                      -0.259544102254463, 0.287479555208803, "contGamma", 0.0406497391363535,
                                      0.426552751857732, 0.35944859178909, 0.154167617583788, -0.0297744482768839,
                                      "debCollin1", 0.142790676592631, 0.35944859178909, 0.388000487607394,
                                      -0.0764961170814851, -0.137220517578011, "contcor1", -0.259544102254463,
                                      0.154167617583788, -0.0764961170814851, 0.556716214776696, 0.247688236920046,
                                      "facFifty", 0.287479555208803, -0.0297744482768839, -0.137220517578011,
                                      0.247688236920046, 0.683577731988681))
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

options <- defaultOptions
options$componentCountMethod <- "eigenValues"
options$variables <- c("contNormal", "contGamma", "contExpon", "contWide", "contNarrow", "contOutlier", "contcor1", "contcor2", "debMiss1", "debCollin1")
jaspTableToRTable <- function(x) do.call(rbind, lapply(x, do.call, what = cbind.data.frame))

allResults <- list(orthogonal = list(
none = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
varimax = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.177837969895818, 0.306520426721584, 0.434926113731913, 0.558690416328051, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.77837969895818, 1.28682456825766, 1.28405687010329, 1.23764302596138, 1.06597010817621),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.177837969895818, 0.128682456825766, 0.128405687010329, 0.123764302596138, 0.106597010817621),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
quartimax = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.178130846180646, 0.307240842252424, 0.435083430695601, 0.558634778448756, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.78130846180646, 1.29109996071778, 1.27842588443178, 1.23551347753155, 1.06652648696916),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.178130846180646, 0.129109996071778, 0.127842588443178, 0.123551347753155, 0.106652648696916),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
bentlerT = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.176362745952805, 0.306703021931096, 0.435942787974185, 0.560164472818831, 0.665287427145673),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.76362745952805, 1.30340275978291, 1.29239766043089, 1.24221684844647, 1.05122954326841),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.176362745952805, 0.130340275978291, 0.129239766043089, 0.124221684844647, 0.105122954326841),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
equamax = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.177944040152155, 0.307025349686837, 0.434932877494347, 0.558472391490633, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.77944040152155, 1.29081309534682, 1.2790752780751, 1.23539513996286, 1.06815035655039),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.177944040152155, 0.129081309534682, 0.12790752780751, 0.123539513996286, 0.106815035655039),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
geominT = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.178687612657789, 0.307214478508799, 0.435421480703313, 0.558922501202021, 0.665287427145673),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.78687612657788, 1.28526865851011, 1.28207002194513, 1.23501020498708, 1.06364925943652),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.178687612657789, 0.128526865851011, 0.128207002194513, 0.123501020498708, 0.106364925943653),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
)),
oblique = list(promax = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.178063770620728, 0.307275203914387, 0.435359955027816, 0.559100125555478, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.78063770620728, 1.29211433293659, 1.28084751113428, 1.23740170527663, 1.06187301590194),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.178063770620728, 0.129211433293659, 0.128084751113429, 0.123740170527663, 0.106187301590194),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
oblimin = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.178064266699675, 0.307137886148764, 0.434885176920807, 0.55857686847834, 0.665287427145673),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.78064266699675, 1.29073619449089, 1.27747290772043, 1.23691691557533, 1.06710558667333),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.178064266699675, 0.129073619449089, 0.127747290772043, 0.123691691557533, 0.106710558667333),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
simplimax = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.180087125491656, 0.316270856776448, 0.444053886518846, 0.564672617070967, 0.665287427145673),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.80087125491656, 1.36183731284792, 1.27783029742398, 1.20618730552121, 1.00614810074705),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.180087125491656, 0.136183731284792, 0.127783029742398, 0.120618730552121, 0.100614810074705),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
bentlerQ = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.177610996940724, 0.307728202686817, 0.435813993111486, 0.559761864011058, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.77610996940724, 1.30117205746093, 1.2808579042467, 1.23947870899571, 1.05525563134615),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.177610996940724, 0.130117205746093, 0.12808579042467, 0.123947870899571, 0.105525563134615),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
biquartimin = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.165658347265737, 0.295950482281138, 0.424227751195743, 0.548282944067767, 0.665287427145673),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.65658347265737, 1.30292135015401, 1.28277268914605, 1.24055192872025, 1.17004483077905),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.165658347265737, 0.130292135015401, 0.128277268914605, 0.124055192872025, 0.117004483077905),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
cluster = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.17852896197966, 0.307610899734307, 0.435233804790158, 0.559076151947334, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.7852896197966, 1.29081937754647, 1.2762290505585, 1.23842347157176, 1.06211275198338),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.17852896197966, 0.129081937754647, 0.12762290505585, 0.123842347157176, 0.106211275198338),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
),
geominQ = data.frame(
  comp = c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"),
  cumpR = c(0.178646355748931, 0.306802818312487, 0.434913837507828, 0.55852912179754, 0.665287427145672),
  cumpU = c(0.179829636044534, 0.316014407729138, 0.443855596638798, 0.564387624608309, 0.665287427145673),
  eigvR = c(1.78646355748931, 1.28156462563556, 1.28111019195341, 1.23615284289712, 1.06758305348132),
  eigvU = c(1.79829636044534, 1.36184771684604, 1.27841188909661, 1.20532027969511, 1.00899802537364),
  propR = c(0.178646355748931, 0.128156462563556, 0.128111019195341, 0.123615284289712, 0.106758305348132),
  propU = c(0.179829636044534, 0.136184771684603, 0.127841188909661, 0.120532027969511, 0.100899802537364)
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
      results <- runAnalysis("principalComponentAnalysis", "test.csv", options, view = FALSE)

      tb <- jaspTableToRTable(results$results$modelContainer$collection$modelContainer_eigenTable$data)

      # allResults[[rotationMethod]][[rotation]] <- tb
      testthat::expect_equal(object = tb, expected = allResults[[rotationMethod]][[rotation]], label = paste(rotationMethod, "-", rotation))

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


# results for PCA based on covariance
options <- defaultOptions
options$variables <- list("contNormal", "contGamma", "contcor1", "contcor2")
options$orthogonalSelector <- "varimax"
options$componentCountMethod <- "manual"
options$analysisBasedOn <- "covarianceMatrix"
set.seed(1)
results <- runAnalysis("principalComponentAnalysis", "test.csv", options)


test_that("Component Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1", 0.445242040210262, 0.445242040210261, 2.44912355022028,
                                      2.44912355022028, 0.445242040210262, 0.445242040210261))
})

test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 2, "Model", 68.91351))
})

test_that("Component Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.167424873032004, 1.09220828001551, "contNormal", 1.46947969487352,
                                      0.188913286087952, "contGamma", -0.412348047289494, 0.853786529139055,
                                      "contcor1", -0.302805178888101, 0.916624916670696, "contcor2"
                                 ))
})


# results for PCA based on mixed matrix (poly or tetrachoric)
options <- defaultOptions
options$variables <- list("contNormal", "contGamma", "debCollin1", "contcor1", "facFive")
options$eigenValuesAbove <- 0.95
options$orthogonalSelector <- "varimax"
options$componentCountMethod <- "parallelAnalysis"
options$parallelAnalysisTable <- TRUE
options$parallelAnalysisSeed <- 1234
options$analysisBasedOn <- "polyTetrachoricCorrelationMatrix"
set.seed(1)
results <- jaspTools::runAnalysis("principalComponentAnalysis", "test.csv", options)

test_that("Component Characteristics table results match for mixed based", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1", 0.28119928428077, 0.28119928428077, 1.40599642140385,
                                      1.40599642140385, 0.28119928428077, 0.28119928428077))
})

test_that("Chi-squared Test table results match for mixed based", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(21.3878764890033, 5, "Model", 0.000684141884394517))
})

test_that("Component Loadings table results match for mixed based", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.468967752780233, 0.780069246852258, "contNormal", -0.644079133313335,
                                      0.585162070030343, "contGamma", 0.277117722692202, 0.923205767769888,
                                      "debCollin1", 0.624793529702314, 0.609633045242123, "contcor1",
                                      0.551422298513999, 0.695933448701539, "facFive"))
})


options <- defaultOptions
options$componentCountMethod <- "parallelAnalysis"
options$parallelAnalysisMethod <- "principalComponentBased"
options$parallelAnalysisTable <- TRUE
options$rotationMethod <- "oblique"
options$variables <- list("contcor1", "contcor2", "facFifty", "facFive","contNormal", "debMiss1")

options("mc.cores" = 1L)
set.seed(1)
results <- runAnalysis("principalComponentAnalysis", "test.csv", options, makeTests = F)

test_that("Parallel Analysis table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parallelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1*", 1.7795916550878, 1.33053625507377, "Component 2*",
                                      1.28644706023115, 1.17402737320915, "Component 3*", 1.08333785331839,
                                      1.0367445878489, "Component 4", 0.848949206589453, 0.923477592629848,
                                      "Component 5", 0.696170865182367, 0.837720518530386, "Component 6",
                                      0.305503359590833, 0.697493672707948))
})


# variance covariance matrix input
dt <- read.csv(testthat::test_path("holzingerswineford.csv"))
cdt <- as.data.frame(cov(dt[, 7:15]))
options <- list(
  addScores = FALSE,
  addedScoresPrefix = "PC",
  analysisBasedOn = "correlationMatrix",
  bartlettTest = FALSE,
  componentCorrelations = FALSE,
  componentCountMethod = "manual",
  loadingsOrder = "sortByVariables",
  dataType = "varianceCovariance",
  eigenValuesAbove = 1,
  kaiserMeyerOlkinTest = FALSE,
  loadingsDisplayLimit = 0.1,
  manualNumberOfComponents = 1,
  mardiaTest = FALSE,
  naAction = "pairwise",
  obliqueSelector = "promax",
  orthogonalSelector = "none",
  parallelAnalysisMethod = "principalComponentBased",
  parallelAnalysisSeed = 1234,
  parallelAnalysisTable = FALSE,
  antiImageCorrelationMatrix = FALSE,
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
results <- runAnalysis("principalComponentAnalysis", cdt, options, makeTests = F)

test_that("Component Characteristics table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_eigenTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Component 1", 0.357371575715301, 3.2163441814377, 0.357371575715301
                                 ))
})

test_that("Chi-squared Test table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_goodnessOfFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(268.944880177337, 27, "Model", 1.0379031932901e-41))
})

test_that("Component Loadings table results match", {
  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_loadingsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.658301970425138, 0.566638515734381, "x1", 0.389685299082607,
                                      0.848145367678899, "x2", 0.477040613001908, 0.772432253546763,
                                      "x3", 0.765811755326015, 0.413532355404489, "x4", 0.737544154070493,
                                      0.456028620796441, "x5", 0.772162747894139, 0.403764690764573,
                                      "x6", 0.348796474669525, 0.878341019258111, "x7", 0.454247183933516,
                                      0.793659495888471, "x8", 0.590666149791766, 0.651113499490171,
                                      "x9"))
})

