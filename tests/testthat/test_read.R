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

#### generate factors TRUE ####

dd <- data.frame(v1 = as.numeric(1:2))
dd$v1 <- factor(x = dd$v1, levels = 1:2, labels = c("one", "2"))

gen_fac <- system.file("extdata", "gen_fac.dta", package="readstata13")



dd118 <- read.dta13(gen_fac, convert.factors = TRUE, generate.factors = TRUE)

test_that("generate.factors TRUE", {
  expect_true(datacompare(dd, dd118))
})


# rm(list = files)


#### noint.factors TRUE ####

dd <- data.frame(v1 = as.numeric(1:2))
dd$v1 <- factor(x = dd$v1, levels = 1:2, labels = c("one", "1.2"))

nonint <- system.file("extdata", "nonint.dta", package="readstata13")



dd118 <- read.dta13(nonint, convert.factors = TRUE, generate.factors = TRUE,
                    nonint.factors = TRUE)

test_that("nonint.factors TRUE", {
  expect_true(datacompare(dd, dd118))
})


# rm(list = files)
