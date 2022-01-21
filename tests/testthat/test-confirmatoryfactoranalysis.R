context("Confirmatory Factor Analysis")

# 3-factor run
options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
options$groupvar <- ""
options$invariance <- "configural"
options$mimic <- "lavaan"
options$se <- "standard"
options$estimator <- "default"
options$std <- "none"
options$factors <- list(
  list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "Factor 1"),
  list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "Factor 2"),
  list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "Factor 3")
)
options$identify <- "factor"
options$missing <- "FIML"
set.seed(1)
results <- jaspTools::runAnalysis("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)


test_that("[CFA 3-Factor] Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.333504675491998, 0.583514923484581, 0.45850979948829, "Factor 1",
                           "<unicode><unicode><unicode>", 6.52589093874667e-13, "Factor 2",
                           0.063779296447443, 7.18900685689003, 0.327797106505315, 0.613272259288624,
                           0.47053468289697, "Factor 1", "<unicode><unicode><unicode>",
                           1.03996145028873e-10, "Factor 3", 0.0728266322838328, 6.46102487704112,
                           0.148278758433621, 0.417692356258714, 0.282985557346168, "Factor 2",
                           "<unicode><unicode><unicode>", 3.83174073319559e-05, "Factor 3",
                           0.0687292215444246, 4.11739797115633))
})

test_that("[CFA 3-Factor] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.741164020131046, 1.05807660760393, 0.899620313867488, "<unicode><unicode>11",
                           "Factor 1", 0, "x1", 0.0808465333987386, 11.1275063512064, 0.346131928251774,
                           0.649749093967046, 0.49794051110941, "<unicode><unicode>12",
                           "Factor 1", 1.28623778294923e-10, "x2", 0.0774547818506273,
                           6.42878979466621, 0.510293170576109, 0.802019014680793, 0.656156092628451,
                           "<unicode><unicode>13", "Factor 1", 0, "x3", 0.0744212256974568,
                           8.81678696472846, 0.878687521771487, 1.1006993764173, 0.989693449094392,
                           "<unicode><unicode>21", "Factor 2", 0, "x4", 0.0566367179185465,
                           17.4744138690689, 0.978762425338572, 1.22444687472433, 1.10160465003145,
                           "<unicode><unicode>22", "Factor 2", 0, "x5", 0.0626757561168699,
                           17.5762482701815, 0.811432261987188, 1.02176969353156, 0.916600977759373,
                           "<unicode><unicode>23", "Factor 2", 0, "x6", 0.0536584940344529,
                           17.0821226769958, 0.483096088879332, 0.75585477823652, 0.619475433557926,
                           "<unicode><unicode>31", "Factor 3", 0, "x7", 0.0695825769015842,
                           8.90273774186456, 0.601768916407813, 0.860128689422337, 0.730948802915075,
                           "<unicode><unicode>32", "Factor 3", 0, "x8", 0.0659093164600047,
                           11.0902197469857, 0.54254918241612, 0.797411035146398, 0.669980108781259,
                           "<unicode><unicode>33", "Factor 3", 0, "x9", 0.0650169734598685,
                           10.3046954222623))
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


# Second-order factor
options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
options$secondOrder <- list("Factor 1", "Factor 2", "Factor 3")
options$groupvar <- ""
options$invariance <- "configural"
options$mimic <- "lavaan"
options$se <- "standard"
options$estimator <- "default"
options$std <- "none"
options$factors <- list(
  list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "Factor 1"),
  list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "Factor 2"),
  list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "Factor 3")
)
options$identify <- "factor"
options$missing <- "FIML"
set.seed(1)
results <- jaspTools::runAnalysis("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)


test_that("[CFA Second order] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.0577039904414908, 0.819337251278242, 0.438520620859867, "<unicode><unicode>11",
                           "Factor 1", 0.0240111123688183, "x1", 0.194297769460158, 2.25695139001474,
                           0.0315641080012427, 0.453878987089273, 0.242721547545258, "<unicode><unicode>12",
                           "Factor 1", 0.0242627980684818, "x2", 0.107735367185111, 2.25294212928438,
                           0.0503061064249717, 0.589382650102084, 0.319844378263528, "<unicode><unicode>13",
                           "Factor 1", 0.0200309356495567, "x3", 0.137522053448247, 2.32576790590095,
                           0.71768085442071, 0.966830831063184, 0.842255842741947, "<unicode><unicode>21",
                           "Factor 2", 0, "x4", 0.0635598354377267, 13.2513848870354, 0.799268757944165,
                           1.07572151104007, 0.937495134492118, "<unicode><unicode>22",
                           "Factor 2", 0, "x5", 0.0705249574167, 13.2930974910468, 0.663204523777401,
                           0.896899759208491, 0.780052141492946, "<unicode><unicode>23",
                           "Factor 2", 0, "x6", 0.0596172269680586, 13.0843412410121, 0.392491072303089,
                           0.651164920657023, 0.521827996480056, "<unicode><unicode>31",
                           "Factor 3", 2.66453525910038e-15, "x7", 0.0659894391923322,
                           7.90775013194371, 0.483529756039699, 0.747933394180798, 0.615731575110249,
                           "<unicode><unicode>32", "Factor 3", 0, "x8", 0.0674511471197128,
                           9.12855602021777, 0.438789772679437, 0.689958088778821, 0.564373930729129,
                           "<unicode><unicode>33", "Factor 3", 0, "x9", 0.0640747274135055,
                           8.80805823935775))
})

test_that("[CFA Second order] Second-order factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-0.149104054075191, 3.73161619230762, 1.79125606911621, "<unicode><unicode>11",
                           "SecondOrder", 0.0703961021446211, "Factor 1", 0.989997846132234,
                           1.80935350123676, 0.364985992598308, 0.869100374746346, 0.617043183672327,
                           "<unicode><unicode>12", "SecondOrder", 1.6021965671964e-06,
                           "Factor 2", 0.128602970800593, 4.79804766430388, 0.360410276598517,
                           0.919053360468363, 0.63973181853344, "<unicode><unicode>13",
                           "SecondOrder", 7.15860527811252e-06, "Factor 3", 0.142513609504142,
                           4.48891737960541))
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


test_that("Bootstrapping works", {
  options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
  options$groupvar <- ""
  options$invariance <- "configural"
  options$mimic <- "lavaan"
  options$se <- "bootstrap"
  options$bootstrapNumber <- 100
  options$estimator <- "default"
  options$std <- "none"
  options$factors <- list(
    list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "Factor 1"),
    list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "Factor 2"),
    list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "Factor 3")
  )
  options$identify <- "factor"
  options$missing <- "FIML"
  set.seed(1)
  results <- jaspTools::runAnalysis("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)

  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.666421502844803, 1.1045952519343, 0.899620313867488, "<unicode>11",
                                      "Factor 1", 0, "x1", 0.0808465333987386, 11.1275063512064, 0.306848270271249,
                                      0.694310421172917, 0.49794051110941, "<unicode>12", "Factor 1",
                                      1.28623778294923e-10, "x2", 0.0774547818506273, 6.42878979466621,
                                      0.447794742651565, 0.843599366253584, 0.656156092628451, "<unicode>13",
                                      "Factor 1", 0, "x3", 0.0744212256974568, 8.81678696472846, 0.873492258837535,
                                      1.10676572310526, 0.989693449094392, "<unicode>21", "Factor 2",
                                      0, "x4", 0.0566367179185465, 17.4744138690689, 1.00613400355279,
                                      1.20780120009224, 1.10160465003145, "<unicode>22", "Factor 2",
                                      0, "x5", 0.0626757561168699, 17.5762482701815, 0.77535379539272,
                                      1.02320525947239, 0.916600977759373, "<unicode>23", "Factor 2",
                                      0, "x6", 0.0536584940344529, 17.0821226769958, 0.399620351789006,
                                      0.750536983553589, 0.619475433557926, "<unicode>31", "Factor 3",
                                      0, "x7", 0.0695825769015842, 8.90273774186456, 0.502036743546884,
                                      0.899496144160653, 0.730948802915075, "<unicode>32", "Factor 3",
                                      0, "x8", 0.0659093164600047, 11.0902197469857, 0.496246268381106,
                                      0.859593369706291, 0.669980108781259, "<unicode>33", "Factor 3",
                                      0, "x9", 0.0650169734598685, 10.3046954222623))
})



# # validate the following tests with:
# library(lavaan)
#
# HS.model <- '
# visual  =~ x1 + x2 + x3
# textual =~ x4 + x5 + x6
# speed   =~ x7 + x8 + x9
# x7 ~~ x8
# g =~ visual + textual + speed
# '
# # Configural model
# fit <- cfa(HS.model, data = HolzingerSwineford1939, effect.coding = TRUE, group = "school")
# summary(fit)

options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
options$groupvar <- "school"
options$invariance <- "configural"
options$mimic <- "lavaan"
options$se <- "standard"
options$estimator <- "default"
options$std <- "none"
options$factors <- list(
  list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "visual"),
  list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "textual"),
  list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "speed")
)
options$identify <- "effects"
options$missing <- "FIML"
options$rescov <-  list(c("x7", "x8"))
options$secondOrder <- list("visual", "textual", "speed")
set.seed(1)

results <- jaspTools::runAnalysis("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)


test_that("Residual covariances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Grant-White"]][["collection"]][["estimates_Grant-White_Residual Covariances"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.208580408543772, 0.561085317759383, 0.384832863151578, "x7",
                                      "<unicode>", 1.87379671678922e-05, "x8", 0.0899263741569042,
                                      4.27942154634322))
})

test_that("Factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Grant-White"]][["collection"]][["estimates_Grant-White_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.97824694709509, 1.41103286870157, 1.19463990789833, "<unicode>112",
                                      "visual", 0, "x1", 0.110406600585583, 10.8203667313558, 0.576588928452012,
                                      1.01820949425935, 0.797399211355682, "<unicode>122", "visual",
                                      1.46349599106088e-12, "x2", 0.112660377764792, 7.07790287212119,
                                      0.803379282477899, 1.21254247901407, 1.00796088074599, "<unicode>132",
                                      "visual", 0, "x3", 0.104380284475531, 9.65662132279655, 0.918525808353015,
                                      1.11584455993924, 1.01718518414613, "<unicode>212", "textual",
                                      0, "x4", 0.0503373411814332, 20.2073681341222, 0.904533581821138,
                                      1.10788011779749, 1.00620684980931, "<unicode>222", "textual",
                                      0, "x5", 0.0518750695370728, 19.3967325497313, 0.875800310154247,
                                      1.07741562193487, 0.97660796604456, "<unicode>232", "textual",
                                      0, "x6", 0.0514334225962675, 18.9878082528273, 0.448709591199949,
                                      0.861628177339534, 0.655168884269742, "<unicode>312", "speed",
                                      4.98220797950921e-10, "x7", 0.105338309631359, 6.21966392438387,
                                      0.632652017321973, 1.05862185080329, 0.845636934062633, "<unicode>322",
                                      "speed", 7.105427357601e-15, "x8", 0.108667770643062, 7.7818559178901,
                                      1.15381004087615, 1.8445783224591, 1.49919418166763, "<unicode>332",
                                      "speed", 0, "x9", 0.176219636440169, 8.50753191842297))
})

test_that("Second-order factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Grant-White"]][["collection"]][["estimates_Grant-White_fl2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.894743205210185, 1.46886037814872, 1.18180179167945, "<unicode>112",
                                      "SecondOrder", 6.66133814775094e-16, "visual", 0.146461153742391,
                                      8.06904603358591, 0.749885842194451, 1.26363322117426, 1.00675953168436,
                                      "<unicode>122", "SecondOrder", 1.57651669496772e-14, "textual",
                                      0.131060413107635, 7.68164473018669, 0.56185444727119, 1.06102290600119,
                                      0.811438676636188, "<unicode>132", "SecondOrder", 1.86384685463281e-10,
                                      "speed", 0.127341232458192, 6.37215975510994))
})

test_that("Factor variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Grant-White"]][["collection"]][["estimates_Grant-White_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.127514015381487, 0.232797490998318, 0.0526417378084156, "visual",
                                      0.566845167322731, 0.0919178896198851, 0.572703942900658, 0.402546923419005,
                                      0.818456895883089, 0.610501909651047, "textual", 8.7184031105636e-09,
                                      0.106101432410169, 5.75394597210484, 0.112134953780972, 0.390602552366561,
                                      0.251368753073767, "speed", 0.000402463070696024, 0.0710389580579302,
                                      3.53846340016394, 0.18878050866914, 0.407359188716625, 0.298069848692882,
                                      "Second-Order", 9.01677925657651e-08, 0.0557608919785278, 5.34550001114871
                                 ))
})

test_that("Residual variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Grant-White"]][["collection"]][["estimates_Grant-White_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.414991134180042, 0.883789180220234, 0.649390157200138, "x1",
                                      5.63612934101343e-08, 0.119593535834845, 5.4299770691363, 0.688644414789547,
                                      1.1677638546569, 0.928204134723222, "x2", 3.10862446895044e-14,
                                      0.122226592847263, 7.5941259025614, 0.406227454069119, 0.787625738823265,
                                      0.596926596446192, "x3", 8.51159587256234e-10, 0.097297268664773,
                                      6.13508071334291, 0.186519967210478, 0.439406093549079, 0.312963030379778,
                                      "x4", 1.2273786600403e-06, 0.0645129523637512, 4.85116583434534,
                                      0.276773271697164, 0.558601647697828, 0.417687459697496, "x5",
                                      6.26294172079156e-09, 0.0718963149893798, 5.80958091884396,
                                      0.274236151148393, 0.545213766052058, 0.409724958600226, "x6",
                                      3.08463521392355e-09, 0.0691282128245982, 5.92702952757996,
                                      0.655173489603016, 1.08413872653021, 0.869656108066612, "x7",
                                      1.99840144432528e-15, 0.109431918216563, 7.94700597631476, 0.556982773635263,
                                      0.99157734143102, 0.774280057533141, "x8", 2.87303514312498e-12,
                                      0.110867998397875, 6.98380117547052, -0.344694371374334, 0.434630459583225,
                                      0.0449680441044453, "x9", 0.821057629495469, 0.198811008035039,
                                      0.226184880550075))
})

test_that("Residual covariances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Pasteur"]][["collection"]][["estimates_Pasteur_Residual Covariances"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.122168637770757, 0.493921275449714, 0.308044956610236, "x7",
                                      "<unicode>", 0.00116151745589432, "x8", 0.0948365991955195,
                                      3.24816536256384))
})

test_that("Factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Pasteur"]][["collection"]][["estimates_Pasteur_fl1"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.10105262470584, 1.66239528529133, 1.38172395499858, "<unicode>111",
                                      "visual", 0, "x1", 0.143202289688303, 9.64875602203059, 0.421600080181834,
                                      0.92883305854499, 0.675216569363412, "<unicode>121", "visual",
                                      1.80752648182292e-07, "x2", 0.129398545678427, 5.21811559645668,
                                      0.700542606262643, 1.18557634501337, 0.943059475638005, "<unicode>131",
                                      "visual", 2.50910403565285e-14, "x3", 0.123735370286549, 7.62158365432657,
                                      0.879064138342627, 1.07622533217526, 0.977644735258943, "<unicode>211",
                                      "textual", 0, "x4", 0.0502971471383697, 19.437379471432, 1.0651533306052,
                                      1.2703138855335, 1.16773360806935, "<unicode>221", "textual",
                                      0, "x5", 0.0523378379772753, 22.3114605646563, 0.764826837719821,
                                      0.944416475623597, 0.854621656671709, "<unicode>231", "textual",
                                      0, "x6", 0.0458145249913662, 18.6539455954801, 0.195451723833617,
                                      0.94752882557255, 0.571490274703083, "<unicode>311", "speed",
                                      0.00289488302453056, "x7", 0.191859928976048, 2.97868490702104,
                                      0.309395340929874, 1.11953466025497, 0.714465000592425, "<unicode>321",
                                      "speed", 0.00054622557003281, "x8", 0.206671991351723, 3.45699964431328,
                                      1.02162210406388, 2.40646734534511, 1.71404472470449, "<unicode>331",
                                      "speed", 1.22372831157236e-06, "x9", 0.353283338929875, 4.85175646804199
                                 ))
})

test_that("Second-order factor loadings table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Pasteur"]][["collection"]][["estimates_Pasteur_fl2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.895669276497663, 2.01755133623086, 1.45661030636426, "<unicode>111",
                                      "SecondOrder", 3.59027865659556e-07, "visual", 0.286199661979113,
                                      5.08948996058061, 0.592187450357759, 1.46800533965735, 1.03009639500755,
                                      "<unicode>121", "SecondOrder", 4.01822369711091e-06, "textual",
                                      0.223427036467998, 4.61043753384383, 0.216566327907607, 0.810020269348763,
                                      0.513293298628185, "<unicode>131", "SecondOrder", 0.000697793364336396,
                                      "speed", 0.151394093494126, 3.39044467839889))
})

test_that("Factor variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Pasteur"]][["collection"]][["estimates_Pasteur_fv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.322593861704548, 0.374611235772924, 0.0260086870341885, "visual",
                                      0.88373996127601, 0.177861711484736, 0.146229825503622, 0.453300232388463,
                                      0.952377694237456, 0.702838963312959, "textual", 3.38341130667885e-08,
                                      0.127318018541578, 5.52034167169694, 0.10185269536012, 0.363320003790462,
                                      0.232586349575291, "speed", 0.000488574525319363, 0.0667020696535148,
                                      3.48694352039548, 0.126201051830929, 0.302878789671742, 0.214539920751336,
                                      "Second-Order", 1.93620086053059e-06, 0.0450716796926947, 4.75997172091434
                                 ))
})

test_that("Residual variances table results match for multiple groups and effects coding", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_Pasteur"]][["collection"]][["estimates_Pasteur_rv"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.145806292617985, 0.80727193782207, 0.476539115220027, "x1",
                                      0.00474239054532877, 0.168744336738236, 2.82403027225296, 0.980200194655344,
                                      1.58852574174061, 1.28436296819797, "x2", 2.22044604925031e-16,
                                      0.155187940156977, 8.27617769073297, 0.661807192404595, 1.1738532769286,
                                      0.917830234666598, "x3", 2.11963779861435e-12, 0.130626401444863,
                                      7.02637617292099, 0.293428309541255, 0.567242028683073, 0.430335169112164,
                                      "x4", 7.24262871898418e-10, 0.0698517220983715, 6.16069520098771,
                                      0.269842486146815, 0.608344243183404, 0.439093364665109, "x5",
                                      3.68010061757573e-07, 0.0863540758163536, 5.08480185230533,
                                      0.195590984648919, 0.394088799248797, 0.294839891948858, "x6",
                                      5.79779713127948e-09, 0.0506381280894964, 5.82248797640715,
                                      0.821118830524024, 1.32986765676015, 1.07549324364209, "x7",
                                      2.22044604925031e-16, 0.129785248670147, 8.28671405003417, 0.583281164346786,
                                      1.02554323626783, 0.80441220030731, "x8", 1.00519592649562e-12,
                                      0.112824030290749, 7.12979494026521, -0.618627025731548, 0.874943807048513,
                                      0.128158390658482, "x9", 0.736602350153651, 0.381019968877275,
                                      0.336356099750146))
})

test_that("Chi-square test table results match for multiple groups and effects coding", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(957.769050808683, 72, "Baseline model", "", 84.655095268701, 46,
                                      "Factor model", 0.000448372893282811))
})
