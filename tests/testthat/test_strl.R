# library(readstata13)
#
# context("strings")
#
# load(system.file("extdata/statacar.RData", package="readstata13"))
#
# test_that("strings are equal", {
#   expect_identical(read.dta13(system.file("extdata/statacar.dta", package="readstata13"))$model, statacar$model)
# })
