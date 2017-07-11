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

#### missings msf/lsf ####
dd <- data.frame(b = as.logical(c(1,NA)),
                 i=as.integer(c(1,NA)),
                 n=as.numeric(c(1,NA)),
                 s=c("1", ""),
                 stringsAsFactors = FALSE)

dd$b <- as.integer(dd$b)

missings_msf <- system.file("extdata", "missings_msf.dta", package="readstata13")
missings_lsf <- system.file("extdata", "missings_lsf.dta", package="readstata13")

dd_msf <- read.dta13(missings_msf)
dd_lsf <- read.dta13(missings_lsf)



test_that("missings msf/lsf", {
  expect_true(datacompare(dd, dd_msf))
  expect_true(datacompare(dd, dd_lsf))
})

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
Encoding(umlauts) <- "UTF-8"

ddcp <- dd <- data.frame(num = factor(1:6, levels = 1:6, labels = umlauts),
                 chr = umlauts, stringsAsFactors = FALSE)

# Dataset in CP1252
levels(ddcp$num)[5:6] <- c("EUR","OE")
ddcp$chr[5:6] <- c("EUR","OE")


# Stata 14
encode <- system.file("extdata", "encode.dta", package="readstata13")
# Stata 12
encodecp <- system.file("extdata", "encodecp.dta", package="readstata13")

ddutf_aE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE,
                       encoding="UTF-8")

# On windows the last two characters will fail on default (not in latin1)
dd_aE <- read.dta13(encode, convert.factors = TRUE, generate.factors = TRUE)

ddcp_aE <- read.dta13(encodecp, convert.factors = TRUE, generate.factors = TRUE)

test_that("encoding CP1252", {
  expect_true(datacompare(ddcp, ddcp_aE))
})

test_that("encoding UTF-8 (Stata 14)", {
  expect_true(datacompare(dd$chr[1:4], dd_aE$chr[1:4]))
  expect_true(datacompare(dd, ddutf_aE))
})

test_that("Reading of strls", {
  strl <- system.file("extdata", "statacar.dta", package="readstata13")

  ddstrlf <- read.dta13(strl, replace.strl = F)
  ddstrlfref <- paste0("11_", 1:8)
  expect_equal(ddstrlf$modelStrL, ddstrlfref)

  ddstrl <- read.dta13(strl, replace.strl = T)
  expect_equal(ddstrl$model, ddstrl$modelStrL)
})
