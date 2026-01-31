context("Example: Mental Ability")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("confirmatoryFactorAnalysis results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Mental Ability.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.264128280987986, 0.552336603203106, 0.408232442095546, "Visual",
     "<unicode>", 2.81808156810115e-08, "Textual", 0.0735238821959155,
     5.5523787632398, 0.15192488585009, 0.37252431824719, 0.26222460204864,
     "Visual", "<unicode>", 3.16849313275469e-06, "Speed", 0.0562763994994704,
     4.65958384653069, 0.0768397261947255, 0.270149642966634, 0.17349468458068,
     "Textual", "<unicode>", 0.000434622709184929, "Speed", 0.0493146604470062,
     3.51811576938906))

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 1, 1, "Visual", "", "jaspColumn1", 0, "", 0.358160250462865,
     0.748840337064458, 0.553500293763662, "Visual", 2.79844001305207e-08,
     "jaspColumn2", 0.0996651187683111, 5.55360090474953, 0.515519121287951,
     0.943221298246137, 0.729370209767044, "Visual", 2.31332730749045e-11,
     "jaspColumn3", 0.109109703120018, 6.68474195154534, 1, 1, 1,
     "Textual", "", "jaspColumn4", 0, "", 0.984855521379058, 1.24129763485795,
     1.11307657811851, "Textual", 0, "jaspColumn5", 0.0654201086095659,
     17.0142881413032, 0.817468475414093, 1.03482399785056, 0.926146236632327,
     "Textual", 0, "jaspColumn6", 0.0554488562419873, 16.7027112802919,
     1, 1, 1, "Speed", "", "jaspColumn7", 0, "", 0.856583100377002,
     1.5033185785647, 1.17995083947085, "Speed", 8.56426041195846e-13,
     "jaspColumn8", 0.164986572020982, 7.15179923442977, 0.785247422575873,
     1.37781289114229, 1.08153015685908, "Speed", 8.39772695826468e-13,
     "jaspColumn9", 0.151167438085725, 7.15451800040272))

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.524214901454325, 1.09441706072732, 0.809315981090822, "Visual",
     2.64043220621346e-08, 0.145462407414288, 5.56374664407848, 0.759767945772752,
     1.19921479994663, 0.979491372859691, "Textual", 0, 0.112105849301359,
     8.73720130540785, 0.214780733047299, 0.552714565480915, 0.383747649264107,
     "Speed", 8.53305080417144e-06, 0.0862091944288759, 4.45135407895159
    ))

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.326400254990787, 0.77170769140725, 0.549053973199019, "jaspColumn1",
     1.34367627646625e-06, 0.113600923264149, 4.83318231421708, 0.934464873024102,
     1.33321315994364, 1.13383901648387, "jaspColumn2", 0, 0.101723370956,
     11.146298100702, 0.666705869811991, 1.021942221605, 0.844324045708497,
     "jaspColumn3", 0, 0.0906231835368076, 9.31686586981978, 0.277647841076288,
     0.464698144597396, 0.371172992836842, "jaspColumn4", 7.32747196252603e-15,
     0.0477177909891552, 7.77850326141875, 0.331807333369721, 0.560702807150411,
     0.446255070260066, "jaspColumn5", 2.1316282072803e-14, 0.0583927754760263,
     7.64229935333833, 0.271855677185058, 0.440549651372841, 0.35620266427895,
     "jaspColumn6", 2.22044604925031e-16, 0.0430349678663536, 8.2770519403,
     0.639886713570462, 0.95889656307974, 0.799391638325101, "jaspColumn7",
     0, 0.0813815590555713, 9.82276141673861, 0.342279337049505,
     0.633114830488821, 0.487697083769163, "jaspColumn8", 4.92252905104351e-11,
     0.0741940912520305, 6.57326042464084, 0.427489444474381, 0.70477314267301,
     0.566131293573695, "jaspColumn9", 1.11022302462516e-15, 0.0707369371033875,
     8.00333343167306))

  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(918.851589292385, 36, "Baseline model", "", 85.3055217699738,
     24, "Factor model", 8.50255321704907e-09))

  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_fits"]][["collection"]][["maincontainer_fits_incrits"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Log-likelihood", -3737.7449266262, "Number of free parameters",
     21, "Akaike (AIC)", 7517.48985325239, "Bayesian (BIC)", 7595.33916881212,
     "Sample-size adjusted Bayesian (SSABIC)", 7528.73911173078
    ))

  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_fits"]][["collection"]][["maincontainer_fits_indices"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Comparative Fit Index (CFI)", 0.930559651799335, "Tucker-Lewis Index (TLI)",
     0.895839477699002, "Bentler-Bonett Non-normed Fit Index (NNFI)",
     0.895839477699002, "Bentler-Bonett Normed Fit Index (NFI)",
     0.907160718048419, "Parsimony Normed Fit Index (PNFI)", 0.60477381203228,
     "Bollen's Relative Fit Index (RFI)", 0.860741077072629, "Bollen's Incremental Fit Index (IFI)",
     0.931490849987257, "Relative Noncentrality Index (RNI)", 0.930559651799335
    ))

  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_fits"]][["collection"]][["maincontainer_fits_others"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Root mean square error of approximation (RMSEA)", 0.0921214845101694,
     "RMSEA 90% CI lower bound", 0.071418490439939, "RMSEA 90% CI upper bound",
     0.113678016811963, "RMSEA p-value", 0.000661236778052077, "Standardized root mean square residual (SRMR)",
     0.0652050571843866, "Hoelter's critical N (<unicode> = .05)",
     129.490200301454, "Hoelter's critical N (<unicode> = .01)",
     152.65402653335, "Goodness of fit index (GFI)", 0.943332061258475,
     "McDonald fit index (MFI)", 0.903177285931361, "Expected cross validation index (ECVI)",
     0.422941932790611))

  plotName <- results[["results"]][["plots"]][["collection"]][["plots_pathplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_model-plot")

})

