library(dod)

test_that("ctd downloads", {
    expect_error(dod.ctd("invalid"), "'program' must be")
    expect_error(dod.ctd("BBMP"), "must give 'year'")
    expect_true(is.data.frame(dod.ctd("BBMP", index=TRUE, year=2022)))
    expect_equal(dod.ctd("BBMP", year="2022", ID="D22667034.ODF"), "./D22667034.ODF")
    expect_error(dod.ctd("BATS"), "Must provide an ID number greater than 10000")
    expect_error(dod.ctd("BATS"), "Must provide an ID number greater than 10000")
    expect_error(dod.ctd("GTSPP"), "must give an ID")
    #expect_equal(dod.ctd("BATS", year="2012", ID="10001"), "./b10001_ctd.txt")
})
