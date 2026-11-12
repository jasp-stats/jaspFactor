context("Confirmatory Factor Analysis")

# 3-factor run
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- ""
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
options$kaiserMeyerOlkinTest <- TRUE
options$bartlettTest <- TRUE

set.seed(1)

results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options)


test_that("[CFA 3-Factor] Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.333504675491998, 0.583514923484581, 0.45850979948829, "Factor 1",
                                      "<unicode>", 6.52589093874667e-13, "Factor 2", 0.063779296447443,
                                      7.18900685689003, 0.327797106505315, 0.613272259288624, 0.47053468289697,
                                      "Factor 1", "<unicode>", 1.03996145028873e-10, "Factor 3", 0.0728266322838328,
                                      6.46102487704112, 0.148278758433621, 0.417692356258714, 0.282985557346168,
                                      "Factor 2", "<unicode>", 3.83174073319559e-05, "Factor 3", 0.0687292215444246,
                                      4.11739797115633))
})

test_that("[CFA 3-Factor] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.741164020131046, 1.05807660760393, 0.899620313867488,
                                      "Factor 1", 0, "x1", 0.0808465333987386, 11.1275063512065, 0.346131928251774,
                                      0.649749093967046, 0.49794051110941,  "Factor 1",
                                      1.28623778294923e-10, "x2", 0.0774547818506273, 6.42878979466621,
                                      0.510293170576109, 0.802019014680793, 0.656156092628451,
                                      "Factor 1", 0, "x3", 0.0744212256974568, 8.81678696472846, 0.878687521771487,
                                      1.1006993764173, 0.989693449094392, "Factor 2",
                                      0, "x4", 0.0566367179185465, 17.4744138690689, 0.978762425338572,
                                      1.22444687472433, 1.10160465003145, "Factor 2",
                                      0, "x5", 0.0626757561168699, 17.5762482701815, 0.811432261987188,
                                      1.02176969353156, 0.916600977759373, "Factor 2",
                                      0, "x6", 0.0536584940344529, 17.0821226769958, 0.483096088879333,
                                      0.75585477823652, 0.619475433557926, "Factor 3",
                                      0, "x7", 0.0695825769015842, 8.90273774186456, 0.601768916407813,
                                      0.860128689422337, 0.730948802915075, "Factor 3",
                                      0, "x8", 0.0659093164600047, 11.0902197469857, 0.54254918241612,
                                      0.797411035146398, 0.669980108781259, "Factor 3",
                                      0, "x9", 0.0650169734598685, 10.3046954222623))
})

test_that("[CFA 3-Factor] Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "Factor 1", "", 0, "", 1, 1, 1, "Factor 2", "", 0, "",
                                      1, 1, 1, "Factor 3", "", 0, ""))
})

test_that("[CFA 3-Factor] Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.326399894319287, 0.771706936936134, 0.549053415627711, "x1",
                                      1.34368055770828e-06, 0.113600822803218, 4.83318168019605, 0.934462587447057,
                                      1.33321007058646, 1.13383632901676, "x2", 0, 0.10172316590628,
                                      11.1462941495686, 0.666706476085054, 1.02194282083783, 0.844324648461441,
                                      "x3", 0, 0.0906231817407956, 9.31687270566615, 0.277647747273574,
                                      0.464697982595281, 0.371172864934427, "x4", 7.32747196252603e-15,
                                      0.047717773591029, 7.77850341710428, 0.331807290171594, 0.560702710555418,
                                      0.446255000363506, "x5", 2.1316282072803e-14, 0.0583927618541264,
                                      7.64229993913142, 0.271855645519897, 0.44054959934542, 0.356202622432658,
                                      "x6", 2.22044604925031e-16, 0.0430349626718039, 8.27705196700539,
                                      0.639885213665674, 0.958894859526553, 0.799390036596114, "x7",
                                      0, 0.0813815071034943, 9.82274800563124, 0.342279857209668,
                                      0.63311496350216, 0.487697410355914, "x8", 4.92208496183366e-11,
                                      0.0741939924882706, 6.57327357646934, 0.427489578257306, 0.704773115754749,
                                      0.566131347006028, "x9", 1.11022302462516e-15, 0.0707368961074339,
                                      8.00333882541577))
})

test_that("[CFA 3-Factor] Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(918.851589292384, 36, "Baseline model", "", 85.305521772505, 24,
                                      "Factor model", 8.50255310602677e-09))
})

test_that("[CFA 3-Factor] Chi-square table footnote reports estimator and test info", {
  footnote <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["footnotes"]][[1]][["text"]]
  testthat::expect_match(footnote, "estimator")
  testthat::expect_match(footnote, "test statistic")
  testthat::expect_match(footnote, "standard error method")
})

test_that("Kaiser-Meyer-Olkin (KMO) test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_kmo"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("x1", 0.805021226632053, "x2", 0.777938285398466, "x3", 0.734302877282004,
                                      "x4", 0.763262045280562, "x5", 0.73872385651986, "x6", 0.807559054803425,
                                      "x7", 0.593046810632567, "x8", 0.682937939231282, "x9", 0.787864851859471,
                                      "Overall", 0.752244592603047))
})

test_that("Bartlett's test of sphericity table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_bartlett"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(904.097051036859, 36, 1.9120788740615e-166))
})


# Second-order factor
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$secondOrder <- list("Factor 1", "Factor 2", "Factor 3")
options$group <- ""
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options)


test_that("[CFA Second order] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0577039904414908, 0.819337251278242, 0.438520620859867,
                                      "Factor 1", 0.0240111123688183, "x1", 0.194297769460158, 2.25695139001474,
                                      0.0315641080012427, 0.453878987089273, 0.242721547545258,
                                      "Factor 1", 0.0242627980684818, "x2", 0.107735367185111, 2.25294212928438,
                                      0.0503061064249717, 0.589382650102084, 0.319844378263528,
                                      "Factor 1", 0.0200309356495567, "x3", 0.137522053448247, 2.32576790590095,
                                      0.71768085442071, 0.966830831063184, 0.842255842741947,
                                      "Factor 2", 0, "x4", 0.0635598354377267, 13.2513848870354, 0.799268757944165,
                                      1.07572151104007, 0.937495134492118, "Factor 2",
                                      0, "x5", 0.0705249574167, 13.2930974910468, 0.663204523777401,
                                      0.896899759208491, 0.780052141492946, "Factor 2",
                                      0, "x6", 0.0596172269680586, 13.0843412410121, 0.392491072303089,
                                      0.651164920657023, 0.521827996480056, "Factor 3",
                                      2.66453525910038e-15, "x7", 0.0659894391923322, 7.90775013194371,
                                      0.483529756039699, 0.747933394180798, 0.615731575110249,
                                      "Factor 3", 0, "x8", 0.0674511471197128, 9.12855602021777, 0.438789772679437,
                                      0.689958088778821, 0.564373930729129, "Factor 3",
                                      0, "x9", 0.0640747274135055, 8.80805823935775))
})

test_that("[CFA Second order] Second-order factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.14910405407519, 3.73161619230762, 1.79125606911621,
                                      "SecondOrder", 0.0703961021446211, "Factor 1", 0.989997846132234,
                                      1.80935350123676, 0.364985992598308, 0.869100374746346, 0.617043183672327,
                                      "SecondOrder", 1.6021965671964e-06, "Factor 2",
                                      0.128602970800593, 4.79804766430388, 0.360410276598517, 0.919053360468363,
                                      0.63973181853344, "SecondOrder", 7.15860527811252e-06,
                                      "Factor 3", 0.142513609504142, 4.48891737960541))
})

test_that("[CFA Second order] Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "Factor 1", "", 0, "", 1, 1, 1, "Factor 2", "", 0, "",
                                      1, 1, 1, "Factor 3", "", 0, "", 1, 1, 1, "Second-Order", "",
                                      0, ""))
})

test_that("[CFA Second order] Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.326401450955574, 0.771708269797423, 0.549054860376498, "x1",
                                      1.34357828596165e-06, 0.11360076571671, 4.83319682673349, 0.934464040890642,
                                      1.33321206655867, 1.13383805372466, "x2", 0, 0.101723304308982,
                                      11.1462959390372, 0.666705522978852, 1.02194196604298, 0.844323744510915,
                                      "x3", 0, 0.0906232068206831, 9.3168601524065, 0.2776477911548,
                                      0.464698082724097, 0.371172936939448, "x4", 7.32747196252603e-15,
                                      0.0477177879401676, 7.77850258701964, 0.331807404885689, 0.560702857278213,
                                      0.446255131081951, "x5", 2.1316282072803e-14, 0.0583927700197611,
                                      7.64230110904024, 0.271855604188449, 0.44054957527582, 0.356202589732134,
                                      "x6", 2.22044604925031e-16, 0.0430349670754176, 8.27705036018498,
                                      0.63988809304374, 0.958898264282657, 0.799393178663198, "x7",
                                      0, 0.0813816411309667, 9.82277043758238, 0.342280111000572,
                                      0.633115637435664, 0.487697874218118, "x8", 4.92219598413612e-11,
                                      0.0741940996694749, 6.57327033269154, 0.427488355941731, 0.704772286642682,
                                      0.566130321292206, "x9", 1.11022302462516e-15, 0.0707369964162943,
                                      8.00331297586447))
})

test_that("[CFA Second order] Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(918.851589292384, 36, "Baseline model", "", 85.3055217707089,
                                      24, "Factor model", 8.50255321704907e-09))
})


# factor loadings with bootstrapping are correct
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- ""
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "bootstrap"
options$bootstrapSamples <- 100
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.664680537078612, 1.08602269196389, 0.899620313867488, "Factor 1", 0, "x1",
                                      0.105577181224036, 8.52097303070146, 0.320608918815302, 0.706659410745969,
                                      0.49794051110941, "Factor 1", 5.99807692402976e-09, "x2", 0.0907909883823308,
                                      5.48447065046288, 0.467399198259181, 0.877475656885832, 0.656156092628451,
                                      "Factor 1", 0, "x3", 0.0906655996475542, 7.23710089801575, 0.856927909068151,
                                      1.10766598642062, 0.989693449094393, "Factor 2", 0, "x4", 0.0599039830242802,
                                      16.5213296199896, 0.974220031073598, 1.19493635386904, 1.10160465003145,
                                      "Factor 2", 0, "x5", 0.0527824491089069, 20.8706619080614, 0.798741904570701,
                                      1.01710747462913, 0.916600977759372, "Factor 2", 0, "x6", 0.0474225661835442,
                                      19.3283715227843, 0.381679674698477, 0.746224780097334, 0.619475433557927,
                                      "Factor 3", 9.55917567324605e-11, "x7", 0.099266429444182, 6.24053304854952,
                                      0.484910983539873, 0.886121366886365, 0.730948802915076, "Factor 3",
                                      2.98294722256287e-12, "x8", 0.106015716344783, 6.89472116132193,
                                      0.499019397467944, 0.910078416523353, 0.669980108781259, "Factor 3",
                                      7.04436509124662e-12, "x9", 0.106712160491033, 6.27838575939577))
})



options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- "school"
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "visual"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "textual"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "speed")
)
options$modelIdentification <- "effectsCoding"
options$residualsCovarying <-  list(c("x7", "x8"))
options$naAction <- "listwise"
options$secondOrder <- list("visual", "textual", "speed")
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options)


test_that("Factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.10105262470584, 1.66239528529133, 1.38172395499858, "Pasteur",
                                      "visual", 0, "x1", 0.143202289688303, 9.64875602203059, 0.421600080181834,
                                      0.92883305854499, 0.675216569363412, "Pasteur", "visual", 1.80752648182292e-07,
                                      "x2", 0.129398545678427, 5.21811559645668, 0.700542606262643,
                                      1.18557634501337, 0.943059475638005, "Pasteur", "visual", 2.50910403565285e-14,
                                      "x3", 0.123735370286549, 7.62158365432657, 0.879064138342627,
                                      1.07622533217526, 0.977644735258943, "Pasteur", "textual", 0,
                                      "x4", 0.0502971471383697, 19.437379471432, 1.0651533306052,
                                      1.2703138855335, 1.16773360806935, "Pasteur", "textual", 0,
                                      "x5", 0.0523378379772753, 22.3114605646563, 0.764826837719821,
                                      0.944416475623597, 0.854621656671709, "Pasteur", "textual",
                                      0, "x6", 0.0458145249913662, 18.6539455954801, 0.195451723833617,
                                      0.94752882557255, 0.571490274703083, "Pasteur", "speed", 0.00289488302453056,
                                      "x7", 0.191859928976048, 2.97868490702105, 0.309395340929874,
                                      1.11953466025498, 0.714465000592425, "Pasteur", "speed", 0.00054622557003281,
                                      "x8", 0.206671991351723, 3.45699964431328, 1.02162210406388,
                                      2.40646734534511, 1.71404472470449, "Pasteur", "speed", 1.22372831157236e-06,
                                      "x9", 0.353283338929875, 4.85175646804199, 0.97824694709509,
                                      1.41103286870157, 1.19463990789833, "Grant-White", "visual",
                                      0, "x1", 0.110406600585583, 10.8203667313558, 0.576588928452012,
                                      1.01820949425935, 0.797399211355682, "Grant-White", "visual",
                                      1.46349599106088e-12, "x2", 0.112660377764792, 7.07790287212119,
                                      0.803379282477899, 1.21254247901407, 1.00796088074599, "Grant-White",
                                      "visual", 0, "x3", 0.104380284475531, 9.65662132279655, 0.918525808353015,
                                      1.11584455993924, 1.01718518414613, "Grant-White", "textual",
                                      0, "x4", 0.0503373411814332, 20.2073681341222, 0.904533581821138,
                                      1.10788011779749, 1.00620684980931, "Grant-White", "textual",
                                      0, "x5", 0.0518750695370728, 19.3967325497312, 0.875800310154247,
                                      1.07741562193487, 0.97660796604456, "Grant-White", "textual",
                                      0, "x6", 0.0514334225962675, 18.9878082528273, 0.448709591199949,
                                      0.861628177339534, 0.655168884269742, "Grant-White", "speed",
                                      4.98220797950921e-10, "x7", 0.105338309631359, 6.21966392438387,
                                      0.632652017321973, 1.05862185080329, 0.845636934062633, "Grant-White",
                                      "speed", 7.105427357601e-15, "x8", 0.108667770643062, 7.7818559178901,
                                      1.15381004087615, 1.8445783224591, 1.49919418166763, "Grant-White",
                                      "speed", 0, "x9", 0.176219636440169, 8.50753191842297))
})

test_that("Second-order factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.895669276497663, 2.01755133623086, 1.45661030636426, "Pasteur",
                                      "SecondOrder", 3.59027865659556e-07, "visual", 0.286199661979113,
                                      5.08948996058061, 0.592187450357759, 1.46800533965735, 1.03009639500755,
                                      "Pasteur", "SecondOrder", 4.01822369711091e-06, "textual", 0.223427036467998,
                                      4.61043753384383, 0.216566327907608, 0.810020269348763, 0.513293298628185,
                                      "Pasteur", "SecondOrder", 0.000697793364336396, "speed", 0.151394093494127,
                                      3.39044467839889, 0.894743205210185, 1.46886037814872, 1.18180179167945,
                                      "Grant-White", "SecondOrder", 6.66133814775094e-16, "visual",
                                      0.146461153742391, 8.06904603358591, 0.749885842194451, 1.26363322117426,
                                      1.00675953168436, "Grant-White", "SecondOrder", 1.57651669496772e-14,
                                      "textual", 0.131060413107635, 7.6816447301867, 0.56185444727119,
                                      1.06102290600119, 0.811438676636188, "Grant-White", "SecondOrder",
                                      1.86384685463281e-10, "speed", 0.127341232458192, 6.37215975510994
                                 ))
})

test_that("Factor variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.322593861704547, 0.374611235772924, 0.0260086870341885, "Pasteur",
                                      "visual", 0.88373996127601, 0.177861711484736, 0.146229825503622,
                                      0.453300232388463, 0.952377694237456, 0.702838963312959, "Pasteur",
                                      "textual", 3.38341130667885e-08, 0.127318018541578, 5.52034167169694,
                                      0.10185269536012, 0.363320003790462, 0.232586349575291, "Pasteur",
                                      "speed", 0.000488574525319363, 0.0667020696535148, 3.48694352039548,
                                      0.126201051830929, 0.302878789671743, 0.214539920751336, "Pasteur",
                                      "Second-Order", 1.93620086053059e-06, 0.0450716796926947, 4.75997172091434,
                                      -0.127514015381487, 0.232797490998318, 0.0526417378084156, "Grant-White",
                                      "visual", 0.566845167322731, 0.0919178896198851, 0.572703942900658,
                                      0.402546923419005, 0.818456895883089, 0.610501909651047, "Grant-White",
                                      "textual", 8.7184031105636e-09, 0.106101432410169, 5.75394597210484,
                                      0.112134953780972, 0.390602552366561, 0.251368753073767, "Grant-White",
                                      "speed", 0.000402463070696024, 0.0710389580579302, 3.53846340016394,
                                      0.188780508669139, 0.407359188716625, 0.298069848692882, "Grant-White",
                                      "Second-Order", 9.01677925657651e-08, 0.0557608919785278, 5.34550001114871
                                 ))
})

test_that("Residual covariances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.122168637770757, 0.493921275449714, 0.308044956610236, "Pasteur",
                                      "x7", "<unicode>", 0.00116151745589432, "x8", 0.0948365991955195,
                                      3.24816536256384, 0.208580408543772, 0.561085317759383, 0.384832863151578,
                                      "Grant-White", "x7", "<unicode>", 1.87379671678922e-05, "x8",
                                      0.0899263741569042, 4.27942154634322))
})


test_that("Residual variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.145806292617985, 0.80727193782207, 0.476539115220027, "Pasteur",
                                      "x1", 0.00474239054532877, 0.168744336738236, 2.82403027225296,
                                      0.980200194655344, 1.58852574174061, 1.28436296819798, "Pasteur",
                                      "x2", 2.22044604925031e-16, 0.155187940156977, 8.27617769073297,
                                      0.661807192404595, 1.1738532769286, 0.917830234666598, "Pasteur",
                                      "x3", 2.11963779861435e-12, 0.130626401444863, 7.02637617292099,
                                      0.293428309541256, 0.567242028683073, 0.430335169112164, "Pasteur",
                                      "x4", 7.24262871898418e-10, 0.0698517220983715, 6.16069520098771,
                                      0.269842486146815, 0.608344243183404, 0.439093364665109, "Pasteur",
                                      "x5", 3.68010061757573e-07, 0.0863540758163536, 5.08480185230533,
                                      0.195590984648919, 0.394088799248797, 0.294839891948858, "Pasteur",
                                      "x6", 5.79779713127948e-09, 0.0506381280894964, 5.82248797640715,
                                      0.821118830524024, 1.32986765676015, 1.07549324364209, "Pasteur",
                                      "x7", 2.22044604925031e-16, 0.129785248670147, 8.28671405003417,
                                      0.583281164346786, 1.02554323626783, 0.80441220030731, "Pasteur",
                                      "x8", 1.00519592649562e-12, 0.112824030290749, 7.12979494026521,
                                      -0.618627025731548, 0.874943807048513, 0.128158390658482, "Pasteur",
                                      "x9", 0.736602350153651, 0.381019968877275, 0.336356099750146,
                                      0.414991134180042, 0.883789180220234, 0.649390157200138, "Grant-White",
                                      "x1", 5.63612934101343e-08, 0.119593535834845, 5.4299770691363,
                                      0.688644414789547, 1.1677638546569, 0.928204134723222, "Grant-White",
                                      "x2", 3.10862446895044e-14, 0.122226592847263, 7.5941259025614,
                                      0.40622745406912, 0.787625738823265, 0.596926596446192, "Grant-White",
                                      "x3", 8.51159587256234e-10, 0.097297268664773, 6.13508071334291,
                                      0.186519967210478, 0.439406093549079, 0.312963030379778, "Grant-White",
                                      "x4", 1.2273786600403e-06, 0.0645129523637512, 4.85116583434534,
                                      0.276773271697164, 0.558601647697828, 0.417687459697496, "Grant-White",
                                      "x5", 6.26294172079156e-09, 0.0718963149893798, 5.80958091884396,
                                      0.274236151148393, 0.545213766052058, 0.409724958600226, "Grant-White",
                                      "x6", 3.08463521392355e-09, 0.0691282128245982, 5.92702952757996,
                                      0.655173489603016, 1.08413872653021, 0.869656108066612, "Grant-White",
                                      "x7", 1.99840144432528e-15, 0.109431918216563, 7.94700597631476,
                                      0.556982773635263, 0.99157734143102, 0.774280057533141, "Grant-White",
                                      "x8", 2.87303514312498e-12, 0.110867998397875, 6.98380117547052,
                                      -0.344694371374334, 0.434630459583225, 0.0449680441044453, "Grant-White",
                                      "x9", 0.821057629495469, 0.198811008035039, 0.226184880550075
                                 ))
})

test_that("Chi-square test table results match for multiple groups and effects coding", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(957.769050808683, 72, "Baseline model", "", 84.655095268701, 46,
                                      "Factor model", 0.000448372893282811))
})



options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$packageMimiced <- "lavaan"
options$seType <- "robust"
options$estimator <- "default"
options$factors <- list(
  list(indicators = list(value = list("V1", "V2", "V3", "V4"), types = c("ordinal", rep("scale", 3))), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("V5", "V6", "V7", "V8"), types = c("ordinal", "scale", "scale", "ordinal")), name = "Factor2", title = "Factor 2")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "pairwise"
options$thresholds <- TRUE
options$group <- "gender"
options$fitMeasures <- TRUE
options$heterotraitMonotraitRatio <- TRUE
dt <- read.csv("cavalini_group.csv")
dt[, c("V1", "V5", "V8")] <- lapply(dt[, c("V1", "V5", "V8")], ordered)

set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", dt, options)


test_that("Thresholds table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Thresholds"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.37345271503234, -1.05964335802485, -1.2165480365286, "f", "V1",
                                      0, "t1", 0.0800548784270476, -15.1964260071572, -0.358874219848855,
                                      -0.118911115849374, -0.238892667849114, "f", "V1", 9.52271353014122e-05,
                                      "t2", 0.0612162024129726, -3.90244181168758, 0.124863894782197,
                                      0.364954328270292, 0.244909111526244, "f", "V1", 6.3717733790325e-05,
                                      "t3", 0.0612486850222499, 3.99860195263418, 0.547166075781201,
                                      0.805483371744301, 0.676324723762751, "f", "V5", 0, "t1", 0.065898480278381,
                                      10.2631308173677, 1.32801878330997, 1.69596338637514, 1.51199108484256,
                                      "f", "V5", 0, "t2", 0.0938651439433244, 16.1081208777083, 1.58695081056115,
                                      2.03777591515951, 1.81236336286033, "f", "V5", 0, "t3", 0.115008517542776,
                                      15.7585142525312, -0.667907099127094, -0.417271559864424, -0.542589329495759,
                                      "f", "V8", 0, "t1", 0.0639388124576908, -8.48607142734779, 0.575854076158956,
                                      0.836151253707481, 0.706002664933219, "f", "V8", 0, "t2", 0.0664035613923819,
                                      10.6320000031537, 1.28086251788209, 1.63744971087504, 1.45915611437857,
                                      "f", "V8", 0, "t3", 0.090967792215996, 16.040359767267, -1.33296389680349,
                                      -1.01400542884757, -1.17348466282553, "m", "V1", 0, "t1", 0.0813684512755931,
                                      -14.4218630738216, -0.342662717731549, -0.0942651168100136,
                                      -0.218463917270781, "m", "V1", 0.000565698466995501, "t2", 0.0633678993289836,
                                      -3.44754867344733, 0.276480375367169, 0.53014885331, 0.403314614338585,
                                      "m", "V1", 4.59333238111981e-10, "t3", 0.0647125355220137, 6.23240321346065,
                                      0.501097035494698, 0.766182966064704, 0.633640000779701, "m",
                                      "V5", 0, "t1", 0.067625204509106, 9.36987925403433, 1.35781354298621,
                                      1.74921140203548, 1.55351247251084, "m", "V5", 0, "t2", 0.0998482273492178,
                                      15.5587386351633, 1.60472242820073, 2.08355527282974, 1.84413885051524,
                                      "m", "V5", 0, "t3", 0.122153480473616, 15.0968997638471, -0.629660973554337,
                                      -0.371855294149489, -0.500758133851913, "m", "V8", 2.66453525910038e-14,
                                      "t1", 0.0657679634519783, -7.61401307822997, 0.682580238630726,
                                      0.961593110504241, 0.822086674567484, "m", "V8", 0, "t2", 0.0711780609425307,
                                      11.549720007563, 1.45747588382256, 1.8793065040716, 1.66839119394708,
                                      "m", "V8", 0, "t3", 0.107611829496968, 15.5037898876544))
})

test_that("Fit indices table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_fits"]][["collection"]][["maincontainer_fits_indices"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Comparative Fit Index (CFI)", 0.837270996361255, "Tucker-Lewis Index (TLI)",
                                      0.760188836742902, "Bentler-Bonett Non-normed Fit Index (NNFI)",
                                      0.760188836742902, "Bentler-Bonett Normed Fit Index (NFI)",
                                      0.814729950768132, "Parsimony Normed Fit Index (PNFI)", 0.648989559539764,
                                      "Bollen's Relative Fit Index (RFI)", 0.726970453763563, "Bollen's Incremental Fit Index (IFI)",
                                      0.839626473453737, "Relative Noncentrality Index (RNI)", 0.837270996361255
                                 ))
})

test_that("Other fit measures table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_fits"]][["collection"]][["maincontainer_fits_others"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Root mean square error of approximation (RMSEA)", 0.112727222947185,
                                      "RMSEA 90% CI lower bound", 0.0992231977494561, "RMSEA 90% CI upper bound",
                                      0.12668058010314, "RMSEA p-value", 1.10023101740353e-13, "Standardized root mean square residual (SRMR)",
                                      0.0652605072256331, "Hoelter's critical N (<unicode> = .05)",
                                      415.284725169067, "Hoelter's critical N (<unicode> = .01)",
                                      475.650388677281, "Goodness of fit index (GFI)", 0.986569391374822,
                                      "McDonald fit index (MFI)", 0.959420192033006, "Expected cross validation index (ECVI)",
                                      ""))
})

test_that("Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1281.53663843542, 56, "Baseline model", "", 237.430356095373,
                                      38, "Factor model", 0))
})

# htmt must use polychoric correlations for the ordinal indicators, see jasp-issues #4434
test_that("Heterotrait-monotrait ratio table results match for ordinal data", {
  table <- results[["results"]][["resHtmtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "", "Factor 1", "f", 0.934652527995396, 1, "Factor 2", "f",
                                      1, "", "Factor 1", "m", 0.977492107173945, 1, "Factor 2", "m"))
})


# scalar invariance with ordinal data: thresholds must be constrained (equal) across groups
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$packageMimiced <- "lavaan"
options$seType <- "robust"
options$estimator <- "default"
options$factors <- list(
  list(indicators = list(value = list("V1", "V2", "V3", "V4"), types = c("ordinal", rep("scale", 3))), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("V5", "V6", "V7", "V8"), types = c("ordinal", "scale", "scale", "ordinal")), name = "Factor2", title = "Factor 2")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "pairwise"
options$thresholds <- TRUE
options$group <- "gender"
options$invarianceTesting <- "scalar"
dt <- read.csv("cavalini_group.csv")
dt[, c("V1", "V5", "V8")] <- lapply(dt[, c("V1", "V5", "V8")], ordered)

set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", dt, options)

test_that("Scalar invariance constrains thresholds across groups for ordinal data", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Thresholds"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1.38439881288148, -1.08715441119612, -1.2357766120388, "f", "V1",
                                      0, "t1", 0.0758290468676944, -16.2968765016257, -0.365430288826985,
                                      -0.152285468903621, -0.258857878865303, "f", "V1", 1.92986813885021e-06,
                                      "t2", 0.0543746776993413, -4.76063288681229, 0.190738587060117,
                                      0.399717611400961, 0.295228099230539, "f", "V1", 3.06388105997257e-08,
                                      "t3", 0.0533119552168417, 5.53774660917471, 0.538460978623922,
                                      0.767492801034327, 0.652976889829125, "f", "V5", 0, "t1", 0.058427558928883,
                                      11.1758372555649, 1.36495526450667, 1.69823183922308, 1.53159355186487,
                                      "f", "V5", 0, "t2", 0.0850210966490339, 18.0142765999276, 1.62947837226539,
                                      2.02723621395554, 1.82835729311047, "f", "V5", 0, "t3", 0.101470701713813,
                                      18.0185734623887, -0.591338386950103, -0.367184745934519, -0.479261566442311,
                                      "f", "V8", 0, "t1", 0.0571831020324045, -8.38117467238351, 0.589277260835033,
                                      0.80431463739021, 0.696795949112621, "f", "V8", 0, "t2", 0.0548574816301126,
                                      12.7019310476355, 1.27841229222241, 1.58684625791402, 1.43262927506822,
                                      "f", "V8", 0, "t3", 0.078683579934247, 18.2074744980517, -1.38439881288148,
                                      -1.08715441119612, -1.2357766120388, "m", "V1", 0, "t1", 0.0758290468676943,
                                      -16.2968765016257, -0.365430288826985, -0.152285468903621, -0.258857878865303,
                                      "m", "V1", 1.92986813885021e-06, "t2", 0.0543746776993413, -4.76063288681229,
                                      0.190738587060117, 0.399717611400961, 0.295228099230539, "m", "V1",
                                      3.06388105997257e-08, "t3", 0.0533119552168417, 5.53774660917471,
                                      0.538460978623923, 0.767492801034327, 0.652976889829125, "m", "V5",
                                      0, "t1", 0.058427558928883, 11.1758372555649, 1.36495526450667,
                                      1.69823183922308, 1.53159355186487, "m", "V5", 0, "t2", 0.0850210966490339,
                                      18.0142765999276, 1.62947837226539, 2.02723621395554, 1.82835729311047,
                                      "m", "V5", 0, "t3", 0.101470701713813, 18.0185734623887, -0.591338386950103,
                                      -0.367184745934519, -0.479261566442311, "m", "V8", 0, "t1", 0.0571831020324045,
                                      -8.38117467238351, 0.589277260835033, 0.80431463739021, 0.696795949112621,
                                      "m", "V8", 0, "t2", 0.0548574816301126, 12.7019310476355, 1.27841229222241,
                                      1.58684625791402, 1.43262927506822, "m", "V8", 0, "t3", 0.078683579934247,
                                      18.2074744980517))
})


# additional output test
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- "sex"
options$invarianceTesting <- "metric"
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "f1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "f2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "f3", title = "Factor 3")
)
options$secondOrder <- list("Factor 1", "Factor 2", "Factor 3")
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
options$averageVarianceExtracted <- TRUE
options$heterotraitMonotraitRatio <- TRUE
options$reliability <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("Average variance extracted table results match", {
  table <- results[["results"]][["resAveTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.305051834487418, "Factor 1", 1, 0.700620392588814, "Factor 2", 1, 0.440212867852147,
                                      "Factor 3", 1, 0.429925397038108, "Factor 1", 2, 0.740548656102918, "Factor 2",
                                      2, 0.400676692760547, "Factor 3", 2))
})

test_that("Heterotrait-monotrait ratio table results match", {
  table <- results[["results"]][["resHtmtTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, "", "", "Factor 1", 1, 0.376412961722238, 1, "", "Factor 2", 1, 0.341052011589284,
                                      0.0950654664695311, 1, "Factor 3", 1, 1, "", "", "Factor 1", 2, 0.438572930736534,
                                      1, "", "Factor 2", 2, 0.446623050626322, 0.355871932355235, 1, "Factor 3",
                                      2))
})

test_that("Reliability table results match", {
  table <- results[["results"]][["resRelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.56769636941697, "Factor 1", 1, 0.535028679477941, 0.872217272102272,
                                      "Factor 2", 1, 0.87499706003875, 0.69693988501424, "Factor 3",
                                      1, 0.702706608978988, 0.720218762931056, "total", 1, 0.843988152680847,
                                      "", "SecondOrder", 1, 0.51731822568291, 0.663613252882612, "Factor 1",
                                      2, 0.676466492554183, 0.892257640765487, "Factor 2", 2, 0.895339124317018,
                                      0.686170258023581, "Factor 3", 2, 0.648172924932421, 0.796625177163525,
                                      "total", 2, 0.832040767424991, "", "SecondOrder", 2, 0.568295638332184
                                 ))
})


# reliability of a single group second-order model
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "f1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "f2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "f3", title = "Factor 3")
)
options$secondOrder <- list("Factor 1", "Factor 2", "Factor 3")
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
options$reliability <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("Reliability table results match for a single group second-order model", {
  table <- results[["results"]][["resRelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.626117131884981, "Factor 1",
                                      0.612005230545227, 0.882706912875796,
                                      "Factor 2", 0.885060891550334,
                                      0.688455022985003, "Factor 3",
                                      0.685841504108865, 0.760488595824792,
                                      "total", 0.859620312056731, "",
                                      "SecondOrder", 0.56366198498232))
})


# reliability of ordinal items: the Holzinger-Swineford items cut into four ordered categories
# semTools 0.5-8 cannot compute the reliability of ordinal items (fixed in 0.5-9)
hsOrdinal <- read.csv(testthat::test_path("holzingerswineford.csv"))
ordinalItems <- paste0("x", 1:9)
hsOrdinal[ordinalItems] <- lapply(hsOrdinal[ordinalItems], function(x)
  cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE, labels = FALSE, ordered_result = TRUE))

options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("ordinal", 3)), name = "f1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("ordinal", 3)), name = "f2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("ordinal", 3)), name = "f3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "pairwise"
options$reliability <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", hsOrdinal, options, makeTests = F)

test_that("Reliability table results match for ordinal items", {
  skip_if(packageVersion("semTools") == "0.5.8", "semTools 0.5-8 cannot compute the reliability of ordinal items")
  table <- results[["results"]][["resRelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.555213848183149, "Factor 1",
                                      0.546655438599964, 0.854297660107689,
                                      "Factor 2", 0.858820989858464,
                                      0.631763742246106, "Factor 3",
                                      0.624561881994531, 0.727667130254379,
                                      "total", 0.8253582728027))
})

options$group <- "sex"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", hsOrdinal, options, makeTests = F)

test_that("Reliability table results match for ordinal items in multiple groups", {
  skip_if(packageVersion("semTools") == "0.5.8", "semTools 0.5-8 cannot compute the reliability of ordinal items")
  table <- results[["results"]][["resRelTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.527339710939714, "Factor 1", 1,
                                      0.510492380714474, 0.833185374702934,
                                      "Factor 2", 1, 0.837118410430062,
                                      0.621128286622363, "Factor 3", 1,
                                      0.671915673678176, 0.675261378312252,
                                      "total", 1, 0.795137021714156,
                                      0.574038175823865, "Factor 1", 2,
                                      0.587419873336518, 0.874171736438173,
                                      "Factor 2", 2, 0.884808661402054,
                                      0.645593775088001, "Factor 3", 2,
                                      0.668116941662998, 0.772109281229804,
                                      "total", 2, 0.854032745752546))
})


# a factor with a single indicator has no reliability of its own, its item stays in the total
# (the analysis refuses such factors, so the coefficients are computed directly)
test_that("Reliability coefficients give NA for a single-indicator factor", {
  dt  <- read.csv(testthat::test_path("holzingerswineford.csv"))
  fit <- lavaan::cfa("f1 =~ x1 + x2 + x3\nf2 =~ x4 + x5 + x6\nf3 =~ x7", data = dt, std.lv = TRUE)
  rel <- jaspFactor:::.cfaReliabilityCoefficients(fit, c("f1", "f2", "f3"))
  expect_equal(colnames(rel[["omega"]]), c("f1", "f2", "f3", "total"))
  expect_true(is.na(rel[["omega"]][1, "f3"]))
  expect_true(is.na(rel[["alpha"]][1, "f3"]))
  expect_equal(unname(rel[["omega"]][1, ]), c(0.612740419333928, 0.884740548042782, NA, 0.868649176557865),
               tolerance = 1e-6)
  expect_equal(unname(rel[["alpha"]][1, ]), c(0.626117131884981, 0.882706912875796, NA, 0.722946933580901),
               tolerance = 1e-6)
})


# structural invariance test
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- "sex"
options$invarianceTesting <- "structural"

options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "f1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "f2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "f3", title = "Factor 3")
)
options$modelIdentification <- "markerVariable"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", "holzingerswineford.csv", options)

test_that("Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.264127499425447, 0.552335580285554, 0.4082315398555, 1, "Factor 1",
                                      "<unicode>", 2.81820449199444e-08, "Factor 2", 0.0735238206246278,
                                      5.55237114158833, 0.15192499892102, 0.372524536760297, 0.262224767840658,
                                      1, "Factor 1", "<unicode>", 3.16848206827203e-06, "Factor 3",
                                      0.0562764263984793, 4.65958456537219, 0.07683979972175, 0.270149836341137,
                                      0.173494818031443, 1, "Factor 2", "<unicode>", 0.000434621849370931,
                                      "Factor 3", 0.0493146910209044, 3.51811629434927, 0.264127499425447,
                                      0.552335580285554, 0.4082315398555, 2, "Factor 1", "<unicode>",
                                      2.81820449199444e-08, "Factor 2", 0.0735238206246278, 5.55237114158833,
                                      0.15192499892102, 0.372524536760297, 0.262224767840658, 2, "Factor 1",
                                      "<unicode>", 3.16848206827203e-06, "Factor 3", 0.0562764263984793,
                                      4.65958456537219, 0.0768397997217501, 0.270149836341137, 0.173494818031443,
                                      2, "Factor 2", "<unicode>", 0.000434621849370931, "Factor 3",
                                      0.0493146910209044, 3.51811629434927))
})

test_that("Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.524213173797399, 1.09441490168121, 0.809314037739305, 1, "Factor 1",
                                      2.64057073984247e-08, 0.145462297364005, 5.56373749353123, 0.759767572610602,
                                      1.19921426302452, 0.97949091781756, 1, "Factor 2", 0, 0.112105807525091,
                                      8.73720050228737, 0.214781179470692, 0.552715570797657, 0.383748375134174,
                                      1, "Factor 3", 8.53300875691687e-06, 0.0862093370063297, 4.45135513692674,
                                      0.524213173797399, 1.09441490168121, 0.809314037739305, 2, "Factor 1",
                                      2.64057073984247e-08, 0.145462297364005, 5.56373749353123, 0.759767572610602,
                                      1.19921426302452, 0.97949091781756, 2, "Factor 2", 0, 0.112105807525091,
                                      8.73720050228737, 0.214781179470692, 0.552715570797656, 0.383748375134174,
                                      2, "Factor 3", 8.53300875691687e-06, 0.0862093370063297, 4.45135513692674
                                 ))
})


# fix manifest intercepts tests
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- "sex"
options$meanStructure <- TRUE
options$meanStructureInterceptFixedToZero <- "manifest"

options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "f1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "f2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "f3", title = "Factor 3")
)
options$modelIdentification <- "markerVariable"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", "holzingerswineford.csv", options)

test_that("Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, 1, "x1", "", 0, "", 0, 0, 0, 1, "x2", "", 0, "", 0, 0,
                                      0, 1, "x3", "", 0, "", 0, 0, 0, 1, "x4", "", 0, "", 0, 0, 0,
                                      1, "x5", "", 0, "", 0, 0, 0, 1, "x6", "", 0, "", 0, 0, 0, 1,
                                      "x7", "", 0, "", 0, 0, 0, 1, "x8", "", 0, "", 0, 0, 0, 1, "x9",
                                      "", 0, "", 0, 0, 0, 2, "x1", "", 0, "", 0, 0, 0, 2, "x2", "",
                                      0, "", 0, 0, 0, 2, "x3", "", 0, "", 0, 0, 0, 2, "x4", "", 0,
                                      "", 0, 0, 0, 2, "x5", "", 0, "", 0, 0, 0, 2, "x6", "", 0, "",
                                      0, 0, 0, 2, "x7", "", 0, "", 0, 0, 0, 2, "x8", "", 0, "", 0,
                                      0, 0, 2, "x9", "", 0, ""))
})

test_that("Factor Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Factor Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.86029752114904, 5.21482119001743, 5.03755935558323, 1, "Factor 1",
                                      0, 0.0904413733274764, 55.6997220436148, 2.75677120171169, 3.10373279405854,
                                      2.93025199788512, 1, "Factor 2", 0, 0.0885122367256859, 33.1056146165016,
                                      3.90407881578928, 4.2206242216325, 4.06235151871089, 1, "Factor 3",
                                      0, 0.0807528628944437, 50.3059752076035, 4.68063900389833, 5.03894993881863,
                                      4.85979447135848, 2, "Factor 1", 0, 0.0914075303797947, 53.1662375207625,
                                      3.03056822444163, 3.39107629675968, 3.21082226060066, 2, "Factor 2",
                                      0, 0.0919680349133189, 34.9123721478544, 4.13410767934369, 4.48973776888174,
                                      4.31192272411271, 2, "Factor 3", 0, 0.0907236286848159, 47.5281113269049
                                 ))
})


options$meanStructureInterceptFixedToZero <- "meanManifest"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", "holzingerswineford.csv", options)

test_that("Factor Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Factor Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4.17946249998224, 7.33554341971368, 5.75750295984796, 1, "Factor 1",
                                      8.61755111714047e-13, 0.805137478195059, 7.15095634692725, 2.67401208793776,
                                      3.38257423864503, 3.02829316329139, 1, "Factor 2", 0, 0.180758972179162,
                                      16.7532107910519, 3.61178718674901, 5.85505485314677, 4.73342101994789,
                                      1, "Factor 3", 2.22044604925031e-16, 0.572272675440052, 8.27126861562646,
                                      5.00292685139523, 6.98910505353057, 5.9960159524629, 2, "Factor 1",
                                      0, 0.506687423289933, 11.833757217676, 2.93332374830556, 3.59818720167768,
                                      3.26575547499162, 2, "Factor 2", 0, 0.169611140463926, 19.2543689409729,
                                      2.91399967225576, 5.16301951397701, 4.03850959311639, 2, "Factor 3",
                                      1.9373391779709e-12, 0.573740094068369, 7.03891820506995))
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-2.29778247855469, 0.848986087595441, -0.724398195479626, 1, "x1",
                                      0.366853737408441, 0.802761834138649, -0.902382456008132, 1.85617988285797,
                                      4.72531692159857, 3.29074840222827, 1, "x2", 6.92600081286798e-06,
                                      0.731936163463202, 4.49595001107458, -4.02924675462711, -1.10345365887017,
                                      -2.56635020674864, 1, "x3", 0.000585265324847928, 0.746389504816217,
                                      -3.4383524824355, -0.436594394646065, 0.215624035960278, -0.110485179342894,
                                      1, "x4", 0.506669795232088, 0.166385310074817, -0.66403205483232,
                                      0.411738355496194, 1.09690396231017, 0.754321158903182, 1, "x5",
                                      1.59186892401131e-05, 0.174790356409219, 4.31557652492663, -0.963464116512774,
                                      -0.324207842607803, -0.643835979560288, 1, "x6", 7.88032553507012e-05,
                                      0.163078576684914, -3.94801078503555, -1.79670881046573, 0.441838146039328,
                                      -0.6774353322132, 1, "x7", 0.235519815460555, 0.571068390583304,
                                      -1.18625955031629, -2.82560377715776, -0.0446667901569227, -1.43513528365734,
                                      1, "x8", 0.0430808812206047, 0.709435736813664, -2.02292499402844,
                                      1.1473885322431, 3.07775269949798, 2.11257061587054, 1, "x9",
                                      1.78730489270862e-05, 0.492448887449298, 4.28992870064723, -2.13315005814143,
                                      -0.170708937633256, -1.15192949788734, 2, "x1", 0.0213944067107943,
                                      0.500631934052784, -2.30095089732308, 1.7331229546958, 3.49858914314841,
                                      2.6158560489221, 2, "x2", 6.31936569739366e-09, 0.450382303547,
                                      5.8080791103932, -2.32301431624246, -0.604838785827062, -1.46392655103476,
                                      2, "x3", 0.000838169615160922, 0.438318138488296, -3.33987216701472,
                                      -0.364138025561973, 0.224025771510095, -0.070056127025939, 2,
                                      "x4", 0.640569850098464, 0.150044542071035, -0.466902201566069,
                                      0.591160705852981, 1.21629568733627, 0.903728196594626, 2, "x5",
                                      1.45442131671558e-08, 0.15947613997356, 5.66685522200663, -1.12225205224369,
                                      -0.545092086893686, -0.833672069568687, 2, "x6", 1.49536132365569e-08,
                                      0.147237390559869, -5.66209484152535, -0.856856508594173, 1.3963872624749,
                                      0.269765376940363, 2, "x7", 0.638850919242091, 0.574817646865548,
                                      0.469306011065214, 0.17776569210359, 2.10083727362224, 1.13930148286292,
                                      2, "x8", 0.0202159309776238, 0.490588499760096, 2.32231591939079,
                                      -2.58679331205368, -0.231340407552876, -1.40906685980328, 2,
                                      "x9", 0.0190291696997005, 0.600891884514286, -2.34495904524032
                                 ))
})


# covariance matrix input
# this test fails because columnIndexInData does not play nice with jaspTools
# 3-factor run
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- ""
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "standard"
options$estimator <- "default"
options$standardized <- "none"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
options$dataType <- "varianceCovariance"
options$sampleSize <- 300

dt <- read.csv(testthat::test_path("holzingerswineford.csv"))
covMatrix <- cov(dt[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")])
covMatrix <- as.data.frame(covMatrix)

set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", covMatrix, options, makeTests = F)


test_that("Chi-square test table results match", {
  skip("Skip test because columnIndexInData does not play nice with jaspTools yet")

  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(915.798926205035, 36, "Baseline model", "", 85.0221147234242,
                                      24, "Factor model", 9.45493439097334e-09))
})


# factor loadings with bootstrapping, standardization
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- ""
options$invarianceTesting <- "configural"
options$packageMimiced <- "lavaan"
options$seType <- "bootstrap"
options$bootstrapSamples <- 100
options$estimator <- "default"
options$standardized <- "all"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)


test_that("Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.275961251949306, 0.602551967160838, 0.45850979948829, "Factor 1",
                                      "<unicode>", 2.8075319846721e-12, "Factor 2", 0.0837953136637843,
                                      5.47178331867089, 0.239097877428563, 0.691004714796994, 0.47053468289697,
                                      "Factor 1", "<unicode>", 4.98415840666233e-06, "Factor 3", 0.111361715590469,
                                      4.22528227409276, 0.0715198869052937, 0.447483820158724, 0.282985557346168,
                                      "Factor 2", "<unicode>", 0.000746849159292129, "Factor 3",
                                      0.0839277224760554, 3.37177691705984))
})

test_that("Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.595967491793905, 0.900594115885398, 0.771880719099138, "Factor 1", 0,
                                      "x1", 0.0789157444118221, 9.78107378764719, 0.274640505096955,
                                      0.547853714221603, 0.423601841658074, "Factor 1", 8.87832030116442e-11, "x2",
                                      0.0676415237892558, 6.26245267593097, 0.418001580565356, 0.752575183341165,
                                      0.58113226322415, "Factor 1", 0, "x3", 0.0727367332997799, 7.98952931841261,
                                      0.793328855048222, 0.906331446634084, 0.851582471792654, "Factor 2", 0, "x4",
                                      0.027747294644135, 30.6906486817682, 0.820218583620474, 0.887438676817449,
                                      0.855065680704751, "Factor 2", 0, "x5", 0.0172467928294704, 49.5782427005014,
                                      0.792995148887663, 0.883954927992625, 0.838010342686877, "Factor 2", 0, "x6",
                                      0.021604786371181, 38.7881800027755, 0.357932725059419, 0.670971563464423,
                                      0.569516154199535, "Factor 3", 2.04725125740879e-13, "x7",
                                      0.0844282764474455, 6.74556177341894, 0.520484028279154, 0.835480417529587,
                                      0.723044461474298, "Factor 3", 2.22044604925031e-16, "x8",
                                      0.0856294035056172, 8.44388062830389, 0.50705881166846, 0.894606901040705,
                                      0.665009552630709, "Factor 3", 5.48450174164827e-14, "x9",
                                      0.0976283726608572, 6.81164229727385))
})

test_that("Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "Factor 1", "", 0, "", 1, 1, 1, "Factor 2", "", 0, "",
                                      1, 1, 1, "Factor 3", "", 0, ""))
})

test_that("Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.188926556872509, 0.644084974483204, 0.404200155482997, "x1",
                                      0.000629256183319882, 0.118232804136795, 3.41868027603692,
                                      0.699829086481015, 0.923970569188726, 0.820561479743888, "x2", 0,
                                      0.0568263095285999, 14.4398164609108, 0.433441683867642, 0.825273899300507,
                                      0.662285292639977, "x3", 0, 0.0855649499080474, 7.74014702692754,
                                      0.178559222293488, 0.370626772804715, 0.274807293735513, "x4",
                                      1.2259127046832e-10, 0.0469170733976437, 5.85729828897031,
                                      0.212452370216918, 0.327205828631691, 0.268862681680922, "x5",
                                      2.91988655476416e-12, 0.0294071056244042, 9.14277947360449,
                                      0.218621625462506, 0.371131728957569, 0.297738665549823, "x6",
                                      6.66133814775094e-16, 0.0361369888343105, 8.23916643733007,
                                      0.549796832822727, 0.871798439892469, 0.675651350105771, "x7", 0,
                                      0.0880515533042568, 7.67336094311816, 0.301964558529721, 0.728971444857033,
                                      0.477206706731343, "x8", 4.04607429709269e-05, 0.116254846715246,
                                      4.10483278946821, 0.199618220269288, 0.742836541376407, 0.557762294909905,
                                      "x9", 4.60190368745117e-06, 0.135696464124458, 4.11036719717571))
})

test_that("Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(918.851589292384, 36, "Baseline model", "", 85.3055217725055,
                                      24, "Factor model", 8.50255310602677e-09))
})


# factor loadings with bootstrapping, standardization and group invariance test
options <- jaspTools::analysisOptions("confirmatoryFactorAnalysis")
options$group <- "school"
options$invarianceTesting <- "scalar"
options$packageMimiced <- "lavaan"
options$seType <- "bootstrap"
options$bootstrapSamples <- 100
options$estimator <- "default"
options$standardized <- "all"
options$modelIdentification <- "markerVariable"
options$factors <- list(
  list(indicators = list(value = list("x1", "x2", "x3"), types = rep("scale", 3)), name = "Factor1", title = "Factor 1"),
  list(indicators = list(value = list("x4", "x5", "x6"), types = rep("scale", 3)), name = "Factor2", title = "Factor 2"),
  list(indicators = list(value = list("x7", "x8", "x9"), types = rep("scale", 3)), name = "Factor3", title = "Factor 3")
)
options$modelIdentification <- "factorVariance"
options$naAction <- "listwise"
set.seed(1)
results <- jaspTools::runAnalysis("confirmatoryFactorAnalysis", testthat::test_path("holzingerswineford.csv"), options, makeTests = F)

test_that("Factor Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Factor Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0, 0, 0, "Pasteur", "Factor 1", "", 0, "", 0, 0, 0, "Pasteur", "Factor 2",
                                      "", 0, "", 0, 0, 0, "Pasteur", "Factor 3", "", 0, "", -0.569445043130582,
                                      0.122446581161433, -0.175535288371218, "Grant-White", "Factor 1",
                                      0.295448082410805, 0.167776518161667, -1.04624467294091, 0.410536098382448,
                                      0.937136380565835, 0.617893927834581, "Grant-White", "Factor 2",
                                      3.03098141918312e-07, 0.130036871851109, 4.7516824961928, -0.47364876209558,
                                      0.0453878307647073, -0.24956546627844, "Grant-White", "Factor 3",
                                      0.0487761628956749, 0.126648064159591, -1.97054307884215))
})

test_that("Intercepts table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Intercepts"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.82537263391805, 4.99215349384842, 4.30190068090687, "Pasteur", "x1", 0,
                                      0.292710748566026, 14.696763620679, 4.46562116692485, 5.54085724695517,
                                      4.92467499942217, "Pasteur", "x2", 0, 0.281098171211864, 17.5194131580118,
                                      1.67775219615152, 2.1353971872555, 1.88486022383063, "Pasteur", "x3", 0,
                                      0.109366806489958, 17.2342988181126, 2.15673889242527, 2.74369913201383,
                                      2.41343151699624, "Pasteur", "x4", 0, 0.134313476409739, 17.9686475364079,
                                      2.82101167888401, 3.58558279958852, 3.18431283764559, "Pasteur", "x5", 0,
                                      0.183134983311491, 17.3877911257919, 1.69649020116505, 2.12459521452555,
                                      1.90007550339723, "Pasteur", "x6", 0, 0.113164727030107, 16.7903511391118,
                                      3.52851507046027, 4.24756866863165, 3.8553172018654, "Pasteur", "x7", 0,
                                      0.189087864501598, 20.389025028271, 5.130821796706, 6.46440399786648,
                                      5.77059511069387, "Pasteur", "x8", 0, 0.3224161143552, 17.897973624037,
                                      4.91825076592137, 6.16325026584029, 5.51562543184978, "Pasteur", "x9", 0,
                                      0.293588346301585, 18.7869358621746, 3.84582331077805, 4.82112368087805,
                                      4.2859787708933, "Grant-White", "x1", 0, 0.247731452145578, 17.3009068237919,
                                      4.88473529607322, 6.43491783003054, 5.61810087972815, "Grant-White", "x2", 0,
                                      0.40442669124218, 13.8915185406591, 2.01208953368605, 2.47534436349833,
                                      2.17351742478477, "Grant-White", "x3", 0, 0.110406783220537,
                                      19.6864482542089, 2.22974116110855, 2.93626467659177, 2.52203984502696,
                                      "Grant-White", "x4", 0, 0.172405979230527, 14.6284940712798, 2.9541178336804,
                                      3.82380946810443, 3.33046740971937, "Grant-White", "x5", 0,
                                      0.207201887650934, 16.0735379753398, 1.55282913787316, 2.01344973485173,
                                      1.76312404048121, "Grant-White", "x6", 0, 0.118927059364688,
                                      14.8252554960987, 3.58125952677721, 4.37803269845174, 3.99057053716559,
                                      "Grant-White", "x7", 0, 0.18669446545506, 21.3748732585015, 4.54936536867853,
                                      6.30418820847833, 5.42204277713541, "Grant-White", "x8", 0,
                                      0.397325612554455, 13.64634598378, 4.59722235574863, 6.03992059978544,
                                      5.36856945405648, "Grant-White", "x9", 0, 0.348122361049197, 15.4215013303836))
})

test_that("Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.201350898178664, 0.662642445356552, 0.489651501809208, "Pasteur",
                                      "Factor 1", "<unicode>", 9.09660323755723e-06, "Factor 2", 0.114114031965344,
                                      4.29089651269103, 0.124456358120698, 0.618099580293298, 0.351445137103768,
                                      "Pasteur", "Factor 1", "<unicode>", 0.00485621809248493, "Factor 3",
                                      0.124784362599085, 2.81641970022247, 0.10724802666031, 0.547056547465775,
                                      0.338229765920924, "Pasteur", "Factor 2", "<unicode>", 0.00130885340283959,
                                      "Factor 3", 0.105235363143, 3.21403144170573, 0.276707208990267,
                                      0.782684735147505, 0.543999435723382, "Grant-White", "Factor 1", "<unicode>",
                                      3.19173893981173e-08, "Factor 2", 0.106664597392124, 5.10009364891251,
                                      0.220397609392973, 0.837358370306263, 0.549563676512851, "Grant-White",
                                      "Factor 1", "<unicode>", 0.00025563174928056, "Factor 3", 0.150295648407129,
                                      3.65655082058107, 0.0295316690572492, 0.604734491502239, 0.356206518510521,
                                      "Grant-White", "Factor 2", "<unicode>", 0.0117744509350264, "Factor 3",
                                      0.141417286475203, 2.51883293329193))
})

test_that("Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.594623334188899, 0.958064917114723, 0.767504218747721, "Pasteur",
                                      "Factor 1", 6.3504757008559e-14, "x1", 0.0979128402909845, 7.83864727513568,
                                      0.302261895924524, 0.564921159257542, 0.411240885120536, "Pasteur",
                                      "Factor 1", 4.90285589904715e-11, "x2", 0.0681393839144913, 6.03528916018093,
                                      0.462318952070999, 0.754527196753023, 0.591249578075409, "Pasteur",
                                      "Factor 1", 1.55431223447522e-15, "x3", 0.0746979309059451, 7.91520689937012,
                                      0.709448824061959, 0.871320437140065, 0.8147367537784, "Pasteur", "Factor 2",
                                      0, "x4", 0.0403201631242525, 20.2066829756534, 0.749937379284218,
                                      0.89546317763352, 0.828893981424851, "Pasteur", "Factor 2", 0, "x5",
                                      0.0370883774114547, 22.3491573176466, 0.790135644756613, 0.910636116518009,
                                      0.862398357717919, "Pasteur", "Factor 2", 0, "x6", 0.0272404139899413,
                                      31.6587830873776, 0.281953225351639, 0.63576177629555, 0.516017953908184,
                                      "Pasteur", "Factor 3", 8.28118018603163e-10, "x7", 0.0804737555061787,
                                      6.41225143107139, 0.40981788350811, 0.787281425969592, 0.657296908755646,
                                      "Pasteur", "Factor 3", 0, "x8", 0.0838242974608593, 7.84136495820395,
                                      0.439146552257307, 0.795935689317652, 0.578220175361584, "Pasteur",
                                      "Factor 3", 9.29146093398003e-10, "x9", 0.0918624007566514, 6.29441611147656,
                                      0.539835464839721, 0.889220494220543, 0.721069217728034, "Grant-White",
                                      "Factor 1", 0, "x1", 0.084048423147289, 8.57921172970028, 0.292459588381161,
                                      0.614629480029429, 0.442399667834903, "Grant-White", "Factor 1",
                                      6.94054780314701e-08, "x2", 0.0829516605801885, 5.33322256288335,
                                      0.443724773628271, 0.831909780977668, 0.642926562525478, "Grant-White",
                                      "Factor 1", 6.90603130237832e-12, "x3", 0.0902161967215157, 7.1265092731641,
                                      0.74690532351169, 0.902358043308646, 0.846861013649664, "Grant-White",
                                      "Factor 2", 0, "x4", 0.0397798206769641, 21.2887086778667, 0.792677775617426,
                                      0.909285694943456, 0.862315742630346, "Grant-White", "Factor 2", 0, "x5",
                                      0.0283597367834456, 30.4063380141703, 0.683367832181481, 0.884215530946679,
                                      0.795972011539709, "Grant-White", "Factor 2", 0, "x6", 0.0469091182264433,
                                      16.968385713356, 0.423332892807918, 0.763139059694378, 0.668495625055567,
                                      "Grant-White", "Factor 3", 8.06021915877864e-14, "x7", 0.0867917995092374,
                                      7.70229017989675, 0.52466004851799, 0.91151555407267, 0.772970300051754,
                                      "Grant-White", "Factor 3", 0, "x8", 0.0891256899044552, 8.67281140690631,
                                      0.536107577513686, 0.917759277980597, 0.704394462211568, "Grant-White",
                                      "Factor 3", 1.99840144432528e-15, "x9", 0.098122240833832, 7.17874414837759))
})

test_that("Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1, 1, 1, "Pasteur", "Factor 1", "", 0, "", 1, 1, 1, "Pasteur",
                                      "Factor 2", "", 0, "", 1, 1, 1, "Pasteur", "Factor 3", "", 0,
                                      "", 1, 1, 1, "Grant-White", "Factor 1", "", 0, "", 1, 1, 1,
                                      "Grant-White", "Factor 2", "", 0, "", 1, 1, 1, "Grant-White",
                                      "Factor 3", "", 0, ""))
})

test_that("Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0819719310777318, 0.646270913831504, 0.410937274204451, "Pasteur", "x1",
                                      0.00668202835435161, 0.151509391687958, 2.71228911703901, 0.680839771371154,
                                      0.908636381021043, 0.830880934405278, "Pasteur", "x2", 0, 0.0595231182193971,
                                      13.9589618161925, 0.430681677224291, 0.786037542808072, 0.650423936425651,
                                      "Pasteur", "x3", 1.28763666396026e-12, 0.0898536114426092, 7.23870666947078,
                                      0.240800176430534, 0.49668117716577, 0.336204022042635, "Pasteur", "x4",
                                      1.30648110108211e-08, 0.0642397709840995, 5.23358064470454,
                                      0.198145069518018, 0.437580167313446, 0.312934767557658, "Pasteur", "x5",
                                      7.82892488615516e-08, 0.0607724051719421, 5.14929048261753,
                                      0.170737644557548, 0.375654347659429, 0.256269072605437, "Pasteur", "x6",
                                      2.6254819207594e-08, 0.0465067313607554, 5.51036516880843, 0.595777376416658,
                                      0.920277888514287, 0.733725471244411, "Pasteur", "x7", 0, 0.0757075112114316,
                                      9.69158092114936, 0.380086832468501, 0.83198013526133, 0.567960773740272,
                                      "Pasteur", "x8", 3.63495855637552e-09, 0.104383748343781, 5.44108429474798,
                                      0.366385392727548, 0.80714766803638, 0.665661428804819, "Pasteur", "x9",
                                      2.30584582538995e-08, 0.113078046484615, 5.88674326714148, 0.209286910885061,
                                      0.708461785636365, 0.480059183245081, "Grant-White", "x1",
                                      8.39349007848433e-05, 0.122063080371016, 3.93287783485326, 0.622208088229512,
                                      0.914447961569039, 0.804282533899567, "Grant-White", "x2", 0,
                                      0.0751039260575127, 10.7089279631489, 0.307911133534369, 0.803104651050219,
                                      0.586645435199173, "Grant-White", "x3", 1.51643337309437e-06,
                                      0.11544171187896, 5.08174580618023, 0.185749422431515, 0.442132397646543,
                                      0.282826423560263, "Grant-White", "x4", 6.23251878550946e-07,
                                      0.0660013947763809, 4.28515828367728, 0.173199520812613, 0.371643313845599,
                                      0.256411560011874, "Grant-White", "x5", 1.01971493471353e-08,
                                      0.0485602431439802, 5.28027751532501, 0.217967762077277, 0.533008400100554,
                                      0.36642855684543, "Grant-White", "x6", 4.58797355662455e-09,
                                      0.0741234900705781, 4.94348763794754, 0.417612722549009, 0.820445242223369,
                                      0.553113599281567, "Grant-White", "x7", 1.05572113051622e-07,
                                      0.105232342204075, 5.25611791676105, 0.169115634832534, 0.724730048299395,
                                      0.402516915237902, "Grant-White", "x8", 0.00201389181570799,
                                      0.130341314441971, 3.08817597061372, 0.157262346005157, 0.712375474276432,
                                      0.503828441605676, "Grant-White", "x9", 0.000479548849835476,
                                      0.144283774051894, 3.49192724487831))
})

test_that("Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(957.769050808683, 72, "Baseline model", "", 164.102830926985,
                                      60, "Factor model", 1.29614097232889e-11))
})
