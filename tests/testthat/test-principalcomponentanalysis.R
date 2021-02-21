context("Principal Component Analysis")

# does not test
# - error handling
# - oblique rotation
# - Parallel analysis / manual
# - slider


# https://jasp-stats.github.io/jasp-verification-project/factor.html
options <- jaspTools::analysisOptions("PrincipalComponentAnalysis")
options$variables <- as.list(c(paste("Question", 1:9, sep="_0"), paste("Question", 10:23, sep="_")))
options$factorMethod <- "parallelAnalysis"
options$rotationMethod <- "orthogonal"
options$basedOn <- "correlation"
options$missingValues <- "pairwise"
options$incl_screePlot <- TRUE

# set.seed(1)
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
    "ref"=list(0.58607525921092, 0.175137117109722, -0.215282966677376, 0.118927425315096,
               0.565352292517859, "Question_01", -0.302587090733824, 0.548328541495602,
               0.1463900031754, 0.00996681877187603, 0.586247492596422, "Question_02",
               -0.628943145853262, 0.290163843470776, 0.213100965307391, -0.0673755834674256,
               0.470283972563951, "Question_03", 0.634461403602982, 0.143506591705323,
               -0.148768059858824, 0.152714314256081, 0.531410988062391, "Question_04",
               0.555546716925549, 0.100876913724159, -0.0741408454958845, 0.136911099089388,
               0.656950179566024, "Question_05", 0.561854260005289, 0.0974058439911111,
               0.571342373038961, -0.0482854194232173, 0.346068303111621, "Question_06",
               0.685184413142295, 0.0391360966114366, 0.252147629029281, 0.103472331853833,
               0.454705735644512, "Question_07", 0.548905592222659, 0.400643553278367,
               -0.322762833711749, -0.416503397400576, 0.260536467171317, "Question_08",
               -0.283846921879958, 0.627027944217558, -0.00830510242467563,
               0.103336207066393, 0.515519535692423, "Question_09", 0.437079957177694,
               0.034531611731784, 0.363097698318426, -0.103447422979793, 0.665227370979451,
               "Question_10", 0.652470118267797, 0.245393290899484, -0.208929369767778,
               -0.400022807059943, 0.310395149829451, "Question_11", 0.668718203619862,
               -0.0477022150289707, 0.0506508285363509, 0.247594491715433,
               0.486671924069512, "Question_12", 0.672989390705788, 0.0757931913743414,
               0.277689588116718, -0.00759007514437021, 0.464171555549614,
               "Question_13", 0.655813914180893, 0.0229599945099146, 0.198319086829362,
               0.135334316119343, 0.511735111298518, "Question_14", 0.592854285225135,
               0.0102589682635061, 0.117161431949699, -0.112621154588887, 0.622008224462925,
               "Question_15", 0.679302991703486, 0.0141666738730523, -0.138138152592824,
               0.0796668970120176, 0.512917787132787, "Question_16", 0.643105512358002,
               0.329513313975546, -0.209622623174979, -0.341618393234928, 0.317191505144432,
               "Question_17", 0.701206278666721, 0.033395163984894, 0.298116541996342,
               0.125135264055858, 0.402662210858605, "Question_18", -0.426810980898239,
               0.389606733236972, 0.0954483671007881, -0.0130729397037431,
               0.656757687466394, "Question_19", 0.43577135234285, -0.205237755778879,
               -0.404471756462646, 0.297287481487613, 0.516003543654907, "Question_20",
               0.657519760814939, -0.055297243837911, -0.186995957582458, 0.282045055687102,
               0.450093077372075, "Question_21", -0.301613785026936, 0.465404111553667,
               -0.115900458876351, 0.377544083448986, 0.536455686315584, "Question_22",
               -0.143940249283243, 0.366462656470356, -0.0211519088146896,
               0.506685519656457, 0.587808706972929, "Question_23")
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
#   jaspTools::expect_equal_plots(testPlot, "scree-plot", dir = "PrincipalComponentAnalysis")
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
  jaspTools::expect_equal_plots(testPlot, "path-diagram", dir = "PrincipalComponentAnalysis")
})

test_that("Scree plot matches", {
  skip("Scree plot check does not work because some data is simulated (non-deterministic).")
  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_scree"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scree-plot", dir = "PrincipalComponentAnalysis")
})
