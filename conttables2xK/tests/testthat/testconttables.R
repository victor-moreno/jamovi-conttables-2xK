testthat::context('conttables')

testthat::test_that('All options in the contTables work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        `x 1` = sample(letters[1:2], 100, replace = TRUE),
        y = sample(LETTERS[1:2], 100, replace = TRUE),
        stringsAsFactors = TRUE,
        check.names = FALSE
    )

    r <- conttables2xK::contTables(
        data=df,
        rows="x 1",
        cols="y",
        chiSqCorr = TRUE,
        zProp = TRUE,
        likeRat = TRUE,
        fisher = TRUE,
        contCoef = TRUE,
        phiCra = TRUE,
        diffProp = TRUE,
        logOdds = TRUE,
        odds = TRUE,
        relRisk = TRUE,
        gamma = TRUE,
        taub = TRUE,
        mh = TRUE,
        exp = TRUE,
        pcRow = TRUE,
        pcCol = TRUE,
        pcTot = TRUE,
        resU = TRUE,
        resP = TRUE,
        resS = TRUE,
        resA = TRUE
    )

    # Test main contingency tables
    mainTable <- r$freqs$asDF
    testthat::expect_equal(c('a', 'b', 'Total'), mainTable[['x 1']])
    testthat::expect_equal(c('Observed', 'Observed', 'Observed'), mainTable[['type[count]']])
    testthat::expect_equal(c('Expected', 'Expected', 'Expected'), mainTable[['type[expected]']])
    testthat::expect_equal(
        c('% within row', '% within row', '% within row'), mainTable[['type[pcRow]']]
    )
    testthat::expect_equal(
        c('% within column', '% within column', '% within column'), mainTable[['type[pcCol]']]
    )
    testthat::expect_equal(c('% of total', '% of total', '% of total'), mainTable[['type[pcTot]']])
    testthat::expect_equal(c(23, 28, 51), mainTable[['1[count]']])
    testthat::expect_equal(c(22.95, 28.05, 51), mainTable[['1[expected]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.511, 0.509, 0.51), mainTable[['1[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.451, 0.549, 1), mainTable[['1[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.23, 0.28, 0.51), mainTable[['1[pcTot]']], tolerance = 1e-3)
    testthat::expect_equal(c(22, 27, 49), mainTable[['2[count]']])
    testthat::expect_equal(c(22.05, 26.95, 49), mainTable[['2[expected]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.489, 0.491, 0.49), mainTable[['2[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.449, 0.551, 1), mainTable[['2[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.22, 0.27, 0.49), mainTable[['2[pcTot]']], tolerance = 1e-3)
    testthat::expect_equal(c(45, 55, 100), mainTable[['.total[count]']])
    testthat::expect_equal(c(45, 55, 100), mainTable[['.total[exp]']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), mainTable[['.total[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.45, 0.55, 1), mainTable[['.total[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.45, 0.55, 1), mainTable[['.total[pcTot]']], tolerance = 1e-3)

    # Test residuals postHoc tables
    postHoc <- r$postHoc$asDF
    testthat::expect_equal(c('Unstandardized residuals', 'Unstandardized residuals'), postHoc[['type[resU]']])
    testthat::expect_equal(c('Pearson residuals', 'Pearson residuals'), postHoc[['type[resP]']])
    testthat::expect_equal(c('Standardized residuals', 'Standardized residuals'), postHoc[['type[resS]']])
    testthat::expect_equal(c('Deviance residuals', 'Deviance residuals'), postHoc[['type[resA]']])
    testthat::expect_equal(c(0.0500, -0.0500), postHoc[['1[resU]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.0500, 0.0500), postHoc[['2[resU]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.0104, -0.0094), postHoc[['1[resP]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.0106, 0.0096), postHoc[['2[resP]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.0201, -0.0201), postHoc[['1[resS]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.0201, 0.0201), postHoc[['2[resS]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.0104, -0.0094), postHoc[['1[resA]']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.0107, 0.0096), postHoc[['2[resA]']], tolerance = 1e-3)

    # Test chi squared tests table
    chiSqTable <- r$chiSq$asDF
    testthat::expect_equal('χ²', chiSqTable[['test[chiSq]']])
    testthat::expect_equal(0, chiSqTable[['value[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal('χ² continuity correction', chiSqTable[['test[chiSqCorr]']])
    testthat::expect_equal(0, chiSqTable[['value[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['p[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal('z test difference in 2 proportions', chiSqTable[['test[zProp]']])
    testthat::expect_equal(0.02, chiSqTable[['value[zProp]']], tolerance = 1e-3)
    testthat::expect_equal(NA, chiSqTable[['df[zProp]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[zProp]']], tolerance = 1e-3)
    testthat::expect_equal('Likelihood ratio', chiSqTable[['test[likeRat]']])
    testthat::expect_equal(0, chiSqTable[['value[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal('Fisher\'s exact test', chiSqTable[['test[fisher]']])
    testthat::expect_equal(NA, chiSqTable[['value[fisher]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['p[fisher]']], tolerance = 1e-3)
    testthat::expect_equal('N', chiSqTable[['test[N]']], tolerance = 1e-3)
    testthat::expect_equal(100, chiSqTable[['value[N]']], tolerance = 1e-3)


    # Test comparative measures table (classic 2x2: a single comparison row).
    # Reference = the first category, for both rows and columns (same as
    # jmv's own contTables) -- OR and DP are therefore numerically identical
    # to jmv's numbers; RR is close but NOT identical (a ratio, unlike OR and
    # DP, is not invariant to the double row+column swap that makes those two
    # match jmv exactly -- see conttables.b.R for the full explanation)
    compMeasuresTable <- r$odds$asDF
    testthat::expect_equal(1, nrow(compMeasuresTable))
    testthat::expect_equal('Difference in 2 proportions', compMeasuresTable[['t[dp]']])
    testthat::expect_equal(0.00202, compMeasuresTable[['v[dp]']], tolerance = 1e-3)
    testthat::expect_equal(-0.195, compMeasuresTable[['cil[dp]']], tolerance = 1e-3)
    testthat::expect_equal(0.199, compMeasuresTable[['ciu[dp]']], tolerance = 1e-3)
    testthat::expect_equal('Log odds ratio', compMeasuresTable[['t[lo]']])
    testthat::expect_equal(0.00808, compMeasuresTable[['v[lo]']], tolerance = 1e-3)
    testthat::expect_equal(-0.78, compMeasuresTable[['cil[lo]']], tolerance = 1e-3)
    testthat::expect_equal(0.796, compMeasuresTable[['ciu[lo]']], tolerance = 1e-3)
    testthat::expect_equal('Odds ratio', compMeasuresTable[['t[o]']])
    testthat::expect_equal(1.008, compMeasuresTable[['v[o]']], tolerance = 1e-3)
    testthat::expect_equal(0.458, compMeasuresTable[['cil[o]']], tolerance = 1e-3)
    testthat::expect_equal(2.217, compMeasuresTable[['ciu[o]']], tolerance = 1e-3)
    testthat::expect_equal('Relative risk', compMeasuresTable[['t[rr]']])
    testthat::expect_equal(1.00413, compMeasuresTable[['v[rr]']], tolerance = 1e-4)
    testthat::expect_equal(0.6717, compMeasuresTable[['cil[rr]']], tolerance = 1e-3)
    testthat::expect_equal(1.5011, compMeasuresTable[['ciu[rr]']], tolerance = 1e-3)


    # Test nominal table
    nominalTable <- r$nom$asDF
    testthat::expect_equal('Contingency coefficient', nominalTable[['t[cont]']])
    testthat::expect_equal(0.002, nominalTable[['v[cont]']], tolerance = 1e-3)
    testthat::expect_equal('Phi-coefficient', nominalTable[['t[phi]']])
    testthat::expect_equal(0.002, nominalTable[['v[phi]']], tolerance = 1e-3)
    testthat::expect_equal('Cramer\'s V', nominalTable[['t[cra]']])
    testthat::expect_equal(0.002, nominalTable[['v[cra]']], tolerance = 1e-3)

    # Test gamma table
    gammaTable <- r$gamma$asDF
    testthat::expect_equal(0.004, gammaTable[['gamma']], tolerance = 1e-3)
    testthat::expect_equal(0.201, gammaTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(-0.39, gammaTable[['cil']], tolerance = 1e-3)
    testthat::expect_equal(0.398, gammaTable[['ciu']], tolerance = 1e-3)

    # Test Kendall's tau table
    tauTable <- r$taub$asDF
    testthat::expect_equal(0.002, tauTable[['taub']], tolerance = 1e-3)
    testthat::expect_equal(0.02, tauTable[['t']], tolerance = 1e-3)
    testthat::expect_equal(0.984, tauTable[['p']], tolerance = 1e-3)

    # Test Mantel-Haenszel test table
    mhTable <- r$mh$asDF
    testthat::expect_equal(0, mhTable[['chi2']], tolerance = 1e-3)
    testthat::expect_equal(1, mhTable[['df']], tolerance = 1e-3)
    testthat::expect_equal(0.984, mhTable[['p']], tolerance = 1e-3)
})

testthat::test_that('conttables works without counts', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    x <- factor(sample(c("A","B"), 100, replace = TRUE), c("A","B"))
    y <- factor(sample(c("I","II"), 100, replace = TRUE), c("I","II"))
    z <- factor(sample(c("foo","bar"), 100, replace = TRUE), c("foo","bar"))
    w <- factor(sample(c("fred","steve"), 100, replace = TRUE), c("fred","steve"))

    data1 <- data.frame(x = x, y = y, z = z, w = w)

    table1<- conttables2xK::contTables(data=data1, rows="x", cols="y")

    freqs1 <- as.data.frame(table1$freqs)

    testthat::expect_equal(28, freqs1[1, '1[count]'])
    testthat::expect_equal(22, freqs1[1, '2[count]'])
    testthat::expect_equal(22, freqs1[2, '1[count]'])
    testthat::expect_equal(28, freqs1[2, '2[count]'])

    table2 <- conttables2xK::contTables(data=data1, rows="x", cols="y", layers=c("z","w"))

    freqs2 <- as.data.frame(table2$freqs)

    testthat::expect_equal(28, freqs2[25, '1[count]'])
    testthat::expect_equal(22, freqs2[25, '2[count]'])
    testthat::expect_equal(22, freqs2[26, '1[count]'])
    testthat::expect_equal(28, freqs2[26, '2[count]'])

    testthat::expect_equal(9, freqs2[10, '1[count]'])
    testthat::expect_equal(4, freqs2[10, '2[count]'])
    testthat::expect_equal(4, freqs2[11, '1[count]'])
    testthat::expect_equal(6, freqs2[11, '2[count]'])
})

testthat::test_that("conttables works with counts", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data <- data.frame(rows = rows, cols = cols, layer = layer, counts = counts)

    table <- conttables2xK::contTables(data=data, rows="rows", cols="cols", layers="layer", counts="counts", resU=TRUE, resP=TRUE, resS=TRUE, resA=TRUE)

    freqs <- as.data.frame(table$freqs)

    testthat::expect_equal(8, freqs[1, '1[count]'])
    testthat::expect_equal(3, freqs[1, '2[count]'])
    testthat::expect_equal(17, freqs[2, '1[count]'])
    testthat::expect_equal(0, freqs[2, '2[count]'])
    testthat::expect_equal(84, freqs[12, '1[count]'])
    testthat::expect_equal(32, freqs[12, '2[count]'])

    # Test residuals postHoc tables
    postHoc <- as.data.frame(table$postHoc)

    testthat::expect_equal('Unstandardized residuals', postHoc[4, 'type[resU]'])
    testthat::expect_equal('Pearson residuals', postHoc[6, 'type[resP]'])
    testthat::expect_equal('Standardized residuals', postHoc[1, 'type[resS]'])
    testthat::expect_equal('Deviance residuals', postHoc[2, 'type[resA]'])
    testthat::expect_equal(2.111, postHoc[4, '1[resU]'], tolerance = 1e-3)
    testthat::expect_equal(1.113, postHoc[6, '2[resP]'], tolerance = 1e-3)
    testthat::expect_equal(-2.422, postHoc[1, '1[resS]'], tolerance = 1e-3)
    testthat::expect_equal(-1.758, postHoc[2, '2[resA]'], tolerance = 1e-3)
})

testthat::test_that("conttables works with global integer weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data <- data.frame(rows = rows, cols = cols, layer = layer)
    attr(data, "jmv-weights") <- counts

    table <- conttables2xK::contTables(data=data, rows="rows", cols="cols", layers="layer")

    freqs <- as.data.frame(table$freqs)

    testthat::expect_equal(8, freqs[1, '1[count]'])
    testthat::expect_equal(3, freqs[1, '2[count]'])
    testthat::expect_equal(17, freqs[2, '1[count]'])
    testthat::expect_equal(0, freqs[2, '2[count]'])
    testthat::expect_equal(84, freqs[12, '1[count]'])
    testthat::expect_equal(32, freqs[12, '2[count]'])
})

testthat::test_that("bar plots work with spaces in variable name", {
    data <- ToothGrowth
    data$dose <- factor(data$dose)
    names(data) <- c("len", "su pp", "do se")

    table <- conttables2xK::contTables(data=data, rows="su pp", cols="do se", barplot=TRUE)

    testthat::expect_true(table$barplot$.render())
})

testthat::test_that("conttables rejects NA counts with a clear error", {
    # GIVEN a 2x2 contingency table where one count value is NA
    rows   <- factor(c("A", "B", "A", "B"), c("A", "B"))
    cols   <- factor(c("1", "1", "2", "2"), c("1", "2"))
    counts <- c(72, 48, 45, NA)
    data   <- data.frame(rows = rows, cols = cols, counts = counts)

    # WHEN running contTables with a counts variable containing NA
    # THEN the analysis is rejected with a clear error message
    testthat::expect_error(
        conttables2xK::contTables(data=data, rows="rows", cols="cols", counts="counts"),
        regexp='missing values',
        ignore.case=TRUE
    )
})

testthat::test_that("generated syntax has an empty formula LHS without counts", {
    # GIVEN a contingency table with rows and columns but no counts variable
    data <- data.frame(
        dose = factor(c("a", "b", "a", "b")),
        supp = factor(c("x", "y", "x", "y"))
    )

    # WHEN running the analysis
    results <- conttables2xK::contTables(data=data, rows="dose", cols="supp")

    # THEN the generated syntax has an empty formula left-hand side rather than
    #   a .COUNTS placeholder
    testthat::expect_match(results$analysis$asSource(), "formula = ~ dose:supp", fixed=TRUE)
})

testthat::test_that("generated syntax uses the counts variable as formula LHS", {
    # GIVEN a contingency table with a counts variable
    data <- data.frame(
        dose = factor(c("a", "b", "a", "b")),
        supp = factor(c("x", "y", "x", "y")),
        freq = c(1, 2, 3, 4)
    )

    # WHEN running the analysis
    results <- conttables2xK::contTables(data=data, rows="dose", cols="supp", counts="freq")

    # THEN the counts variable appears as the formula's left-hand side
    testthat::expect_match(results$analysis$asSource(), "formula = freq ~ dose:supp", fixed=TRUE)
})

# ---- 2xK comparative measures (new behaviour vs. jmv) --------------------
#
# `compare` selects the variable whose two categories are "the compared
# groups" (it must have exactly two categories). The other variable can have
# K >= 2 categories; each non-reference category is contrasted, against a
# reference category (its first level), between the two compared groups.
# Reference = the first category, for BOTH axes (rows and columns) -- same
# convention as jmv's own contTables. This makes OR K-invariant (filtering a
# 2xK table down to just one category reproduces the same OR, since OR only
# looks at the {reference, category} pair) and, for a true 2x2 table (K == 2),
# makes OR and DP numerically identical to jmv's original formula (a
# simultaneous row+column swap is a 180-degree rotation of the table, which
# these two measures are invariant to). RR is NOT invariant to that swap (it's
# a ratio, not a difference), so RR can differ from jmv's original number even
# for a true 2x2 table. RR/DP use each compared group's FULL row total as the
# denominator (matching the "% within column/row" reading from the frequency
# table), which is NOT K-invariant (changes when other categories are
# added/removed) -- so RR/DP can, in principle, still disagree in sign with OR
# for a K > 2 table if the other categories skew the row totals enough (a
# Simpson's-paradox-style effect, not a bug).

testthat::test_that("2xK comparative measures expand to one row per non-reference category", {
    # GIVEN a 2x3 table: 2-level status (the compared groups) x 3-level dose
    # (the iterated variable), compare="rows" so `status` (2 levels) is the
    # compared variable and `dose` (K=3) is iterated vs. its reference (low)
    data <- data.frame(
        status = factor(rep(c("neg", "pos"), 3), c("neg", "pos")),
        dose   = factor(c("low","low","med","med","high","high"), c("low","med","high")),
        n      = c(50, 10, 40, 20, 30, 25)
    )

    r <- conttables2xK::contTables(
        data=data, rows="status", cols="dose", counts="n", compare="rows",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)

    odds <- r$odds$asDF

    # one row per non-reference category of `dose` (med, high), vs. the
    # reference category (low, the first level)
    testthat::expect_equal(2, nrow(odds))
    testthat::expect_equal(c("med", "high"), odds[['dose']])

    # OR/logOR: classic 2x2 odds ratio of the {reference, category} x
    # {compared groups} submatrix, e.g. for "med": (50*20)/(40*10) = 2.5
    testthat::expect_equal(c(2.5, 4.166667), odds[['v[o]']], tolerance = 1e-5)

    # DP/RR (genuine 2xK, K=3 > 2): compares each dose category's prevalence
    # between the two status groups, using each group's FULL row total (all
    # dose categories), e.g. for "med": P(med|pos) - P(med|neg) = 20/55 - 40/120 = 0.0303
    testthat::expect_equal(c(0.03030303, 0.20454545), odds[['v[dp]']], tolerance = 1e-5)
    testthat::expect_equal(c(1.090909, 1.818182), odds[['v[rr]']], tolerance = 1e-5)

    # compare="columns" is invalid here (dose has 3 levels, not 2, so it
    # can't be "the compared groups") -- single unavailable placeholder row
    rBad <- conttables2xK::contTables(
        data=data, rows="status", cols="dose", counts="n", compare="columns",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)
    oddsBad <- rBad$odds$asDF
    testthat::expect_equal(1, nrow(oddsBad))
    testthat::expect_true(is.nan(oddsBad[['v[dp]']]))
    testthat::expect_true(is.nan(oddsBad[['v[o]']]))
})

testthat::test_that("OR for a reference-vs-category pair is unchanged if the other 2xK categories are dropped", {
    # GIVEN the same 2x3 table as above, but with the "high" dose category
    # removed (a true 2x2 of just status x {low, med}): OR for "med" must
    # equal the OR reported for "med" in the full 2x3 table, since OR is
    # always the classic 2x2 odds ratio of the {reference, category}
    # submatrix alone and must not depend on what other categories exist
    data3 <- data.frame(
        status = factor(rep(c("neg", "pos"), 3), c("neg", "pos")),
        dose   = factor(c("low","low","med","med","high","high"), c("low","med","high")),
        n      = c(50, 10, 40, 20, 30, 25)
    )
    odds3 <- conttables2xK::contTables(
        data=data3, rows="status", cols="dose", counts="n", compare="rows",
        logOdds=TRUE, odds=TRUE)$odds$asDF

    data2 <- data3[data3$dose != "high", ]
    data2$dose <- factor(data2$dose, c("low", "med"))
    odds2 <- conttables2xK::contTables(
        data=data2, rows="status", cols="dose", counts="n", compare="rows",
        logOdds=TRUE, odds=TRUE)$odds$asDF

    testthat::expect_equal(odds3[odds3$dose == 'med', 'v[o]'], odds2[['v[o]']])
    testthat::expect_equal(odds3[odds3$dose == 'med', 'v[lo]'], odds2[['v[lo]']])
})

testthat::test_that("true 2x2 tables reproduce jmv's original OR and DP exactly, RR is close but not exact", {
    # a classic 2x2 table (both variables have exactly 2 levels): OR must be
    # identical whichever variable is nominated as "compare" (it's
    # orientation-invariant); OR and DP are numerically identical to jmv's
    # own contTables formula (success = first/reference category); RR is not
    # (a ratio isn't invariant to the row+column swap that makes OR and DP
    # match jmv exactly) -- these are jmv's own textbook numbers for this
    # exact 2x2 table (status x dose, compare="rows"/"columns")
    data <- data.frame(
        status = factor(c("neg","neg","pos","pos"), c("neg","pos")),
        dose   = factor(c("low","high","low","high"), c("low","high")),
        n      = c(50, 30, 10, 25)
    )

    rRows <- conttables2xK::contTables(
        data=data, rows="status", cols="dose", counts="n", compare="rows",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)
    rCols <- conttables2xK::contTables(
        data=data, rows="status", cols="dose", counts="n", compare="columns",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)

    oddsRows <- rRows$odds$asDF
    oddsCols <- rCols$odds$asDF

    testthat::expect_equal(oddsRows[['v[o]']],  oddsCols[['v[o]']])
    testthat::expect_equal(oddsRows[['v[lo]']], oddsCols[['v[lo]']])
    testthat::expect_false(isTRUE(all.equal(oddsRows[['v[rr]']], oddsCols[['v[rr]']])))

    # compare="rows": success = dose 'high', reference 'low'; effect group =
    # status 'pos' (2nd level), reference group = 'neg' (1st level):
    # P(high|pos) - P(high|neg) = 25/35 - 30/80 = 0.3392857 -- numerically
    # equal to jmv's own P(low|neg) - P(low|pos), by the row+column symmetry
    testthat::expect_equal(4.166667, oddsRows[['v[o]']], tolerance = 1e-5)
    testthat::expect_equal(0.3392857, oddsRows[['v[dp]']], tolerance = 1e-5)
    testthat::expect_equal(1.904762, oddsRows[['v[rr]']], tolerance = 1e-5)
    # compare="columns": success = status 'pos', reference 'neg'; effect
    # group = dose 'high' (2nd level), reference group = 'low' (1st level):
    # P(pos|high) - P(pos|low) = 25/55 - 10/60 = 0.2878788
    testthat::expect_equal(0.2878788, oddsCols[['v[dp]']], tolerance = 1e-5)
    testthat::expect_equal(2.727273, oddsCols[['v[rr]']], tolerance = 1e-5)
})

testthat::test_that("comparative measures stay unavailable for RxC tables where no axis has 2 levels", {
    data <- data.frame(
        rows = factor(rep(c("A","B","C"), 3), c("A","B","C")),
        cols = factor(rep(c("x","y","z"), each=3), c("x","y","z")),
        n    = c(10,12,8, 9,11,13, 7,14,10)
    )

    r <- conttables2xK::contTables(
        data=data, rows="rows", cols="cols", counts="n",
        diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)

    odds <- r$odds$asDF
    testthat::expect_equal(1, nrow(odds))
    testthat::expect_true(is.nan(odds[['v[dp]']]))
})
