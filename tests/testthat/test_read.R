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
  strl <- system.file("extdata", "statacar_strl.dta", package="readstata13")
  
  ddstrlf <- read.dta13(strl, replace.strl = F)
  ddstrlfref <- c("00000000130000000001",  "00000000130000000002",  "00000000130000000003",  "00000000130000000004",  "00000000130000000005",  "00000000130000000006",  "00000000130000000007",  "00000000130000000008",  "00000000130000000009",  "00000000130000000010",  "00000000130000000011",  "00000000130000000012",  "00000000130000000013",  "00000000130000000014",  "00000000130000000015",  "00000000130000000016",  "00000000130000000017",  "00000000130000000018",  "00000000130000000019",  "00000000130000000020",  "00000000130000000021",  "00000000130000000022",  "00000000130000000023",  "00000000130000000024",  "00000000130000000025",  "00000000130000000026",  "00000000130000000027",  "00000000130000000028",  "00000000130000000029",  "00000000130000000030",  "00000000130000000031",  "00000000130000000032",  "00000000130000000033",  "00000000130000000034",  "00000000130000000035",  "00000000130000000036",  "00000000130000000037",  "00000000130000000038",  "00000000130000000039",  "00000000130000000040",  "00000000130000000041",  "00000000130000000042",  "00000000130000000043",  "00000000130000000044",  "00000000130000000045",  "00000000130000000046",  "00000000130000000047",  "00000000130000000048",  "00000000130000000049",  "00000000130000000050",  "00000000130000000051",  "00000000130000000052",  "00000000130000000053",  "00000000130000000054",  "00000000130000000055",  "00000000130000000056",  "00000000130000000057",  "00000000130000000058",  "00000000130000000059",  "00000000130000000060",  "00000000130000000061",  "00000000130000000062",  "00000000130000000063",  "00000000130000000064",  "00000000130000000065",  "00000000130000000066",  "00000000130000000067",  "00000000130000000068",  "00000000130000000069",  "00000000130000000070",  "00000000130000000071",  "00000000130000000072",  "00000000130000000073",  "00000000130000000074")
  expect_equal(ddstrlf$mymake, ddstrlfref)
  
  ddstrl <- read.dta13(strl, replace.strl = T)
  expect_equal(ddstrl$mymake, ddstrl$make)
})

