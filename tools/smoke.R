# Smoke test, run by tools/install.sh right after installing the module into
# jamovi desktop and/or the docker container (the runner has put jamovi's
# libraries and the installed module on .libPaths() and attached it).
# Moved here from the docker target of the previous install.sh.

# 2x3 table: 2-level status (the compared groups) x 3-level dose (iterated)
data <- data.frame(
    status = factor(rep(c("neg", "pos"), 3), c("neg", "pos")),
    dose   = factor(c("low","low","med","med","high","high"), c("low","med","high")),
    n      = c(50, 10, 40, 20, 30, 25)
)
r <- conttables2xK::contTables(
    data=data, rows="status", cols="dose", counts="n", compare="rows",
    diffProp=TRUE, logOdds=TRUE, odds=TRUE, relRisk=TRUE)

odds <- r$odds$asDF
stopifnot(nrow(odds) == 2)
stopifnot(identical(odds[["dose"]], c("med", "high")))
stopifnot(isTRUE(all.equal(odds[["v[o]"]][1], 2.5)))
cat(sprintf("   2xK comparative-measures smoke test passed (OR[med vs low] = %.3f)\n", odds[["v[o]"]][1]))
