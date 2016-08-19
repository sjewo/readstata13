library(readstata13)

context("Reading datasets")


datacompare <- function(x, y) {
  res <- unlist(Map(all.equal, x, y))

  # with all(unlist(res)) if not TRUE, a warning is thrown
  res <- all(unlist(lapply(res, isTRUE)))

  res
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

#### encoding TRUE ####

umlauts <- c("ä","ö","ü","ß")

dd <- data.frame(num = factor(1:4, levels = 1:4, labels = umlauts),
                 chr = umlauts, stringsAsFactors = FALSE)

encode <- system.file("extdata", "encode.dta", package="readstata13")

# no Encoding and with Encoding
dd_nE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE)
dd_wE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE,
                    encoding = "CP1252")

test_that("encoding CP1252", {
  expect_false(datacompare(dd, dd_nE))
  expect_true(datacompare(dd, dd_wE))
})


# rm(list = files)
