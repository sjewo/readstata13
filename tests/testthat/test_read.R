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

#### generate factors FALSE ####

dd <- data.frame(v1 = as.numeric(1:2))

gen_fac <- system.file("extdata", "gen_fac.dta", package="readstata13")

suppressWarnings(dd118 <- read.dta13(gen_fac, convert.factors = TRUE, generate.factors = FALSE))

test_that("generate.factors TRUE", {
  expect_true(datacompare(dd, dd118))
})


#### convert.underscore = TRUE ####

dd <- data.frame(v.1 = as.numeric(1:2), 
                 v.2  = as.numeric(1:2),
                 long.name.multiple.underscores = as.numeric(1:2))

underscore <- system.file("extdata", "underscore.dta", package="readstata13")

dd118 <- read.dta13(underscore, convert.underscore = T)

test_that("generate.factors TRUE", {
  expect_true(datacompare(dd, dd118))
})

#### convert.underscore = FALSE ####

dd <- data.frame(v.1 = as.numeric(1:2), 
                 v.2  = as.numeric(1:2),
                 long_name_multiple_underscores = as.numeric(1:2))

underscore <- system.file("extdata", "underscore.dta", package="readstata13")

dd118 <- read.dta13(underscore, convert.underscore = F)

test_that("generate.factors TRUE", {
  expect_true(datacompare(dd, dd118))
})


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

umlauts <- c("ä","ö","ü","ß","€","Œ")

ddutf <- ddcp <- dd <- data.frame(num = factor(1:6, levels = 1:6, labels = umlauts),
                 chr = umlauts, stringsAsFactors = FALSE)

# Dataset in CP1252
ddcp$chr <- iconv(dd$chr, to="CP1252")
ddcp$chr[5:6] <- iconv(c("EUR","OE"), to="CP1252")
levels(ddcp$num) <- iconv(levels(dd$num), to="CP1252")
levels(ddcp$num)[5:6] <- iconv(c("EUR","OE"), to="CP1252")

# Dataset in UTF-8
ddutf$chr <- iconv(dd$chr, to="UTF-8")
levels(ddutf$num) <- iconv(levels(dd$num), to="UTF-8")

# Stata 14
encode <- system.file("extdata", "encode.dta", package="readstata13")
# Stata 12
encodecp <- system.file("extdata", "encodecp.dta", package="readstata13")

# no Encoding and with Encoding
# works on modern linux because stata 14 uses utf-8
dd_nE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE,
                    encoding = NULL)
# This should work on windows
ddcp_nE <- read.dta13(encodecp, convert.factors = TRUE, generate.factors = TRUE,
                    encoding = NULL)

# This should work on windows and modern linux
dd_fE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE,
                    fromEncoding = "UtF-8")
ddcp_fE <- read.dta13(encodecp, convert.factors = TRUE, generate.factors = TRUE,
                     fromEncoding = "CP1252")
dd_aE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE)
ddcp_aE <- read.dta13(encodecp, convert.factors = TRUE, generate.factors = TRUE)

# This should not work on windows and linux
dd_nfE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE,
                    fromEncoding = "CP1252")
ddcp_nfE <- read.dta13(encodecp, convert.factors = TRUE, generate.factors = TRUE,
                      fromEncoding = "UtF-8")


test_that("encoding CP1252", {
  if(.Platform$OS.type == "unix") {
  expect_false(datacompare(ddcp, ddcp_nE))
  } else {
    expect_true(datacompare(ddcp, ddcp_nE))
  }
  expect_true(datacompare(ddcp, ddcp_fE))
  expect_true(datacompare(ddcp, ddcp_aE))
  expect_false(datacompare(ddcp, ddcp_nfE))
})

test_that("encoding UTF-8 (Stata 14)", {
  expect_true(datacompare(dd, dd_nE))
  expect_true(datacompare(dd, dd_fE))
  expect_true(datacompare(dd, dd_aE))
  expect_false(datacompare(dd, dd_nfE))
})

# rm(list = files)
