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

test_that("reading of many strls", {

  # slow test
  N = 1e4
  big_strl <- data.frame(
    x = 1:N,
    y = sample(LETTERS, N, replace = TRUE),
    z = c(paste(rep("a", 3000), collapse=""), sample(LETTERS, N-1, replace=TRUE))
  )

  # writing the file is slow
  if (!file.exists("big_strl.dta"))
    readstata13::save.dta13(big_strl, "big_strl.dta")

  expect_silent(x <- readstata13::read.dta13("big_strl.dta", select.rows = 1))
  unlink("big_strl.dta")

})

test_that("various datetime conversions", {
  datetime <- system.file("extdata", "datetime.dta", package="readstata13")

  td       <- c("2001-05-15",
                "1999-04-01",
                "1975-11-15",
                "1960-08-26",
                "1987-12-16")
  tc       <- c("2011-06-25 05:15:06",
                "2011-03-13 08:30:45",
                "2011-04-09 10:17:08",
                "2012-02-11 10:30:12",
                "2012-08-01 06:45:59")
  tc_hh_mm <- c("2011-06-29 10:27:00",
                "2011-03-26 02:15:00",
                "2011-04-09 19:35:00",
                "2012-02-16 02:15:00",
                "2012-08-02 11:59:00")
  ty       <- c("2011-01-01",
                "2011-01-01",
                "2011-01-01",
                "2012-01-01",
                "2012-01-01")
  tm       <- c("2011-06-01",
                "2011-03-01",
                "2011-04-01",
                "2012-02-01",
                "2012-08-01")
  tq       <- c("2011-04-01",
                "2011-01-01",
                "2011-04-01",
                "2012-01-01",
                "2012-07-01")

  dd <- data.frame(td = as.Date(td),
                   tc = as.POSIXct(tc, tz = "GMT"),
                   tc_hh_mm = as.POSIXct(tc_hh_mm, tz = "GMT"),
                   ty = as.Date(ty),
                   tm = as.Date(tm),
                   tq = as.Date(tq))
  dddates <- read.dta13(datetime, convert.dates = TRUE)
  expect_true(all.equal(dd, dddates, check.attributes = FALSE))
})

test_that("reading file format 120 works", {

  fl <- system.file("extdata", "myproject2.dtas", package="readstata13")

  tmp <- tempdir()

  fls <- unzip(fl, exdir = tmp)

  # data name, dta file name, dta version
  data_fram <- strsplit(readLines(fls[1])[-c(1:2)], " ")
  data_fram <- as.data.frame(do.call("rbind", data_fram))

  expect_equal(data_fram$V1, c("persons", "counties"))

  # read dtas
  dtas <- fls[tools::file_ext(fls) == "dta"]
  expect_equal(basename(dtas), paste0(data_fram$V2, ".dta"))

  expect_warning(
    df1 <- read.dta13(dtas[1]),
    "File contains unhandled alias variable in column: 5"
  )
  df2 <- read.dta13(dtas[2], convert.factors = FALSE)

  expect_equal(attr(df1, "version"), as.integer(data_fram$V3[1]))
  expect_equal(attr(df2, "version"), as.integer(data_fram$V3[2]))

  # backup order
  nams <- names(df1)

  # merge: fralias_from in attr(df1, "expansion.fields") tells what to merge
  df <- merge(
    df1[-which(names(df1) == "median")],
    df2,
    by = "countyid",
    all.x = TRUE
  )

  # update names
  as_name <- attr(df1, "expansion.fields")[[16]]
  nams2 <- names(df)
  nams2[nams2 == as_name[3]] <- as_name[1]
  names(df) <- nams2

  # resore expected order
  df <- df[nams]

  # restore order
  df <- df[order(df$personid), ]

  expect_equal(
    df$personid, 1:20
  )

  expect_equal(
    c("personid", "countyid", "income", "counties", "median", "ratio"),
    names(df)
  )

  
  # read all frames in myproject2.dtas
  expect_warning(
    dtas1 <- read.dtas(fl),
    "File contains unhandled alias variable in column: 5")
  
  expect_equal(
    c("persons", "counties"),
    names(dtas1)
  )
  
  # read selected frames
  expect_warning(
    dtas2 <- read.dtas(fl, select.frames = c("persons", "counties")),
    "File contains unhandled alias variable in column: 5")
  
  expect_equal(
    c("persons", "counties"),
    names(dtas2)
  )
  
  # read only frame counties
  dtas3 <- read.dtas(fl, select.frames = c("counties"))

  expect_equal(
    "counties",
    names(dtas3)
  )
  
  # read frames with different arguments
  dtas4 <- read.dtas(fl, 
            read.dta13.options = list(counties = list(select.cols = "median_income"),
                                      persons = list(select.cols = "income")))
  
  expect_equal(names(dtas4$persons), "income")  
  expect_equal(names(dtas4$counties), "median_income")

  # read frames with different arguments
  dtas5 <- read.dtas(fl, 
                     read.dta13.options = list(persons = list(select.cols = c("income", "countyid"))))
  
  expect_equal(ncol(dtas5$persons), 2)  
  expect_equal(names(dtas5$persons), c("countyid", "income"))
  
})
