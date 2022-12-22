library(dod)
library(testthat)

test_that("ctd downloads", {
    expect_error(dod.ctd("invalid"), "'program' must be")
    expect_error(dod.ctd("BBMP"), "Must provide an ID")
    expect_equal("./2022667ODFSUMMARY.tsv", dod.ctd(program="BBMP", index=TRUE, year=2022))
    expect_silent(dod.ctd(program="BBMP", year="2022", ID="D22667034.ODF"))
    expect_error(dod.ctd("BATS"), "ID must be supplied")
    expect_error(dod.ctd("BATS", "junk"), "ID must be a numeric value")
    expect_error(dod.ctd("BATS", 1), "ID must exceed 10000")
    expect_error(dod.ctd("GTSPP"), "must give an ID")
    expect_silent(dod.ctd("GTSPP", ID="at198501"))
})
