library(readstata13)

context("Reading datasets")


datacompare <- function(x, y) {
  all(unlist(Map(all.equal, x, y)))
}



#### missings ####
# missings.do creates missings.dta
# missings.dta contains variable missings containing ., .a, .b, ..., .z
#
# Note: prior Stata 8 there was only a single missing value

dd <- data.frame(missings = as.numeric(rep(NA, 27)))

missings <- system.file("extdata", "missings.dta", package="readstata13")



dd118 <- read.dta13(missings, missing.type = FALSE)

dd118_m <- read.dta13(missings, missing.type = TRUE)

mvals <- attr(dd118_m, "missing")$missings

test_that("missings", {
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd118_m))
  expect_identical(mvals, as.numeric(0:26))
})


# rm(list = files)
