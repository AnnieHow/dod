library(dod)

test_that("ctd downloads", {
    expect_error(dod.ctd("invalid"), "'program' must be")
    expect_error(dod.ctd("BBMP"), "must give 'year'")
})
