library(dod)
library(testthat)

test_that("ctd downloads", {
    expect_error(dod.ctd("invalid"), "'program' must be")
    expect_error(dod.ctd("BBMP"), "must give 'year'")
    expect_true(is.data.frame(dod.ctd(program="BBMP", index=TRUE, year=2022, read=TRUE)))
    expect_silent(dod.ctd(program="BBMP", year="2022", ID="D22667034.ODF"))
    expect_error(dod.ctd("BATS"), "Must provide an ID number greater than 10000")
    expect_error(dod.ctd("BATS"), "Must provide an ID number greater than 10000")
    expect_error(dod.ctd("GTSPP"), "must give an ID")
    #expect_equal(dod.ctd("BATS", year="2012", ID="10001"), "./b10001_ctd.txt")
    expect_equal(dod.ctd("GTSPP", ID="at198501"), "./at198501.gz")
})
