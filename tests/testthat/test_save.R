library(readstata13)

context("Saving datasets")

# ToDo: Fix this.
# load(system.file("extdata/statacar.RData", package="readstata13"))
#
# saveandload <- function(x, ...) {
#   file <- tempfile(pattern="readstata13_", fileext=".dta")
#   save.dta13(x, file=file,  ...)
#   all(unlist(Map(identical, x, read.dta13(file))))
# }
#
# test_that("Saved file is identical: Version 118", {
#             expect_true(saveandload(statacar, version="118", convert.factors=T))
# })



datacompare <- function(x, y) {
  all(unlist(Map(all.equal, x, y)))
}

namescompare <- function(x, y){
  all(identical(names(x), names(y)))
}


files <- c("dd118", "dd117", "dd115", "dd114", "dd113", "dd112", "dd111",
           "dd110", "dd108", "dd107", "dd106", "dd105", "dd104", "dd103",
           "dd102", "dd")

data(mtcars)

#### version ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_118.dta", version = 118)
save.dta13(dd, "data/dta_117.dta", version = 117)
save.dta13(dd, "data/dta_115.dta", version = 115)
save.dta13(dd, "data/dta_114.dta", version = 114)
save.dta13(dd, "data/dta_113.dta", version = 113)
save.dta13(dd, "data/dta_112.dta", version = 112)
save.dta13(dd, "data/dta_111.dta", version = 111)
save.dta13(dd, "data/dta_110.dta", version = 110)
save.dta13(dd, "data/dta_108.dta", version = 108)
save.dta13(dd, "data/dta_107.dta", version = 107)
save.dta13(dd, "data/dta_106.dta", version = 106)
save.dta13(dd, "data/dta_105.dta", version = 105)
save.dta13(dd, "data/dta_104.dta", version = 104)
save.dta13(dd, "data/dta_103.dta", version = 103)
save.dta13(dd, "data/dta_102.dta", version = 102)

dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
dd106 <- read.dta13("data/dta_106.dta")
dd105 <- read.dta13("data/dta_105.dta")
dd104 <- read.dta13("data/dta_104.dta")
dd103 <- read.dta13("data/dta_103.dta")
dd102 <- read.dta13("data/dta_102.dta")

# rm -r
unlink("data", recursive = TRUE)

test_that("version", {
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  expect_true(datacompare(dd, dd106))
  expect_true(datacompare(dd, dd105))
  expect_true(datacompare(dd, dd104))
  expect_true(datacompare(dd, dd103))
  expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### compress ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_118.dta", version = 118, compress = TRUE)
save.dta13(dd, "data/dta_117.dta", version = 117, compress = TRUE)
save.dta13(dd, "data/dta_115.dta", version = 115, compress = TRUE)
save.dta13(dd, "data/dta_114.dta", version = 114, compress = TRUE)
save.dta13(dd, "data/dta_113.dta", version = 113, compress = TRUE)
save.dta13(dd, "data/dta_112.dta", version = 112, compress = TRUE)
save.dta13(dd, "data/dta_111.dta", version = 111, compress = TRUE)
save.dta13(dd, "data/dta_110.dta", version = 110, compress = TRUE)
save.dta13(dd, "data/dta_108.dta", version = 108, compress = TRUE)
save.dta13(dd, "data/dta_107.dta", version = 107, compress = TRUE)
save.dta13(dd, "data/dta_106.dta", version = 106, compress = TRUE)
save.dta13(dd, "data/dta_105.dta", version = 105, compress = TRUE)
save.dta13(dd, "data/dta_104.dta", version = 104, compress = TRUE)
save.dta13(dd, "data/dta_103.dta", version = 103, compress = TRUE)
save.dta13(dd, "data/dta_102.dta", version = 102, compress = TRUE)

dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
dd106 <- read.dta13("data/dta_106.dta")
dd105 <- read.dta13("data/dta_105.dta")
dd104 <- read.dta13("data/dta_104.dta")
dd103 <- read.dta13("data/dta_103.dta")
dd102 <- read.dta13("data/dta_102.dta")

# rm -r
unlink("data", recursive = TRUE)

test_that("compress", {
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  expect_true(datacompare(dd, dd106))
  expect_true(datacompare(dd, dd105))
  expect_true(datacompare(dd, dd104))
  expect_true(datacompare(dd, dd103))
  expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### convert.factors TRUE ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars
dd$am <- factor(x = dd$am, levels = c(0,1), labels = c("auto", "man"))


save.dta13(dd, "data/dta_118.dta", version = 118, convert.factors = TRUE)
save.dta13(dd, "data/dta_117.dta", version = 117, convert.factors = TRUE)
save.dta13(dd, "data/dta_115.dta", version = 115, convert.factors = TRUE)
save.dta13(dd, "data/dta_114.dta", version = 114, convert.factors = TRUE)
save.dta13(dd, "data/dta_113.dta", version = 113, convert.factors = TRUE)
save.dta13(dd, "data/dta_112.dta", version = 112, convert.factors = TRUE)
save.dta13(dd, "data/dta_111.dta", version = 111, convert.factors = TRUE)
save.dta13(dd, "data/dta_110.dta", version = 110, convert.factors = TRUE)
save.dta13(dd, "data/dta_108.dta", version = 108, convert.factors = TRUE)
save.dta13(dd, "data/dta_107.dta", version = 107, convert.factors = TRUE)
# save.dta13(dd, "data/dta_106.dta", version = 106, convert.factors = TRUE)
# save.dta13(dd, "data/dta_105.dta", version = 105, convert.factors = TRUE)
# save.dta13(dd, "data/dta_104.dta", version = 104, convert.factors = TRUE)
# save.dta13(dd, "data/dta_103.dta", version = 103, convert.factors = TRUE)
# save.dta13(dd, "data/dta_102.dta", version = 102, convert.factors = TRUE)


dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
# dd106 <- read.dta13("data/dta_106.dta")
# dd105 <- read.dta13("data/dta_105.dta") no factors
# dd104 <- read.dta13("data/dta_104.dta")
# dd103 <- read.dta13("data/dta_103.dta")
# dd102 <- read.dta13("data/dta_102.dta")

# rm -r
unlink("data", recursive = TRUE)

test_that("convert.factors TRUE", {
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  # expect_true(datacompare(dd, dd106))
  # expect_true(datacompare(dd, dd105)) no factors
  # expect_true(datacompare(dd, dd104))
  # expect_true(datacompare(dd, dd103))
  # expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### convert.factors FALSE ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars
dd$am <- factor(x = dd$am, levels = c(0,1), labels = c("auto", "man"))


save.dta13(dd, "data/dta_118.dta", version = 118, convert.factors = FALSE)
save.dta13(dd, "data/dta_117.dta", version = 117, convert.factors = FALSE)
save.dta13(dd, "data/dta_115.dta", version = 115, convert.factors = FALSE)
save.dta13(dd, "data/dta_114.dta", version = 114, convert.factors = FALSE)
save.dta13(dd, "data/dta_113.dta", version = 113, convert.factors = FALSE)
save.dta13(dd, "data/dta_112.dta", version = 112, convert.factors = FALSE)
save.dta13(dd, "data/dta_111.dta", version = 111, convert.factors = FALSE)
save.dta13(dd, "data/dta_110.dta", version = 110, convert.factors = FALSE)
save.dta13(dd, "data/dta_108.dta", version = 108, convert.factors = FALSE)
save.dta13(dd, "data/dta_107.dta", version = 107, convert.factors = FALSE)
# save.dta13(dd, "data/dta_106.dta", version = 106, convert.factors = FALSE)
# save.dta13(dd, "data/dta_105.dta", version = 105, convert.factors = FALSE) # no factors | expect_warning ?
# save.dta13(dd, "data/dta_104.dta", version = 104, convert.factors = FALSE)
# save.dta13(dd, "data/dta_103.dta", version = 103, convert.factors = FALSE)
# save.dta13(dd, "data/dta_102.dta", version = 102, convert.factors = FALSE)


dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
# dd106 <- read.dta13("data/dta_106.dta")
# dd105 <- read.dta13("data/dta_105.dta") no factors | expect_warning ?
# dd104 <- read.dta13("data/dta_104.dta")
# dd103 <- read.dta13("data/dta_103.dta")
# dd102 <- read.dta13("data/dta_102.dta")

# add one (because of stupid factor)
dd <- mtcars
dd$am <- dd$am + 1

# rm -r
unlink("data", recursive = TRUE)

test_that("convert.factors TRUE", {
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  # expect_true(datacompare(dd, dd106))
  # expect_true(datacompare(dd, dd105)) no factors
  # expect_true(datacompare(dd, dd104))
  # expect_true(datacompare(dd, dd103))
  # expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### add rownames TRUE ####
if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_118.dta", version = 118, add.rownames = TRUE)
save.dta13(dd, "data/dta_117.dta", version = 117, add.rownames = TRUE)
save.dta13(dd, "data/dta_115.dta", version = 115, add.rownames = TRUE)
save.dta13(dd, "data/dta_114.dta", version = 114, add.rownames = TRUE)
save.dta13(dd, "data/dta_113.dta", version = 113, add.rownames = TRUE)
save.dta13(dd, "data/dta_112.dta", version = 112, add.rownames = TRUE)
save.dta13(dd, "data/dta_111.dta", version = 111, add.rownames = TRUE)
save.dta13(dd, "data/dta_110.dta", version = 110, add.rownames = TRUE)
save.dta13(dd, "data/dta_108.dta", version = 108, add.rownames = TRUE)
save.dta13(dd, "data/dta_107.dta", version = 107, add.rownames = TRUE)
save.dta13(dd, "data/dta_106.dta", version = 106, add.rownames = TRUE)
save.dta13(dd, "data/dta_105.dta", version = 105, add.rownames = TRUE)
save.dta13(dd, "data/dta_104.dta", version = 104, add.rownames = TRUE)
save.dta13(dd, "data/dta_103.dta", version = 103, add.rownames = TRUE)
save.dta13(dd, "data/dta_102.dta", version = 102, add.rownames = TRUE)


dd118 <- read.dta13("data/dta_118.dta", add.rownames = TRUE)
dd117 <- read.dta13("data/dta_117.dta", add.rownames = TRUE)
dd115 <- read.dta13("data/dta_115.dta", add.rownames = TRUE)
dd114 <- read.dta13("data/dta_114.dta", add.rownames = TRUE)
dd113 <- read.dta13("data/dta_113.dta", add.rownames = TRUE)
dd112 <- read.dta13("data/dta_112.dta", add.rownames = TRUE)
dd111 <- read.dta13("data/dta_111.dta", add.rownames = TRUE)
dd110 <- read.dta13("data/dta_110.dta", add.rownames = TRUE)
dd108 <- read.dta13("data/dta_108.dta", add.rownames = TRUE)
dd107 <- read.dta13("data/dta_107.dta", add.rownames = TRUE)
dd106 <- read.dta13("data/dta_106.dta", add.rownames = TRUE)
dd105 <- read.dta13("data/dta_105.dta", add.rownames = TRUE)
dd104 <- read.dta13("data/dta_104.dta", add.rownames = TRUE)
dd103 <- read.dta13("data/dta_103.dta", add.rownames = TRUE)
dd102 <- read.dta13("data/dta_102.dta", add.rownames = TRUE)

# rm -r
unlink("data", recursive = TRUE)

test_that("add.rownames TRUE", {
  # Check that rownames are identical
  expect_true(identical(rownames(dd), rownames(dd118)))
  expect_true(identical(rownames(dd), rownames(dd117)))
  expect_true(identical(rownames(dd), rownames(dd115)))
  expect_true(identical(rownames(dd), rownames(dd114)))
  expect_true(identical(rownames(dd), rownames(dd113)))
  expect_true(identical(rownames(dd), rownames(dd112)))
  expect_true(identical(rownames(dd), rownames(dd111)))
  expect_true(identical(rownames(dd), rownames(dd110)))
  expect_true(identical(rownames(dd), rownames(dd108)))
  expect_true(identical(rownames(dd), rownames(dd107)))
  expect_true(identical(rownames(dd), rownames(dd106)))
  expect_true(identical(rownames(dd), rownames(dd105)))
  expect_true(identical(rownames(dd), rownames(dd104)))
  expect_true(identical(rownames(dd), rownames(dd103)))
  expect_true(identical(rownames(dd), rownames(dd102)))

  # Check that data is identical
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  expect_true(datacompare(dd, dd106))
  expect_true(datacompare(dd, dd105))
  expect_true(datacompare(dd, dd104))
  expect_true(datacompare(dd, dd103))
  expect_true(datacompare(dd, dd102))
})

# rm(list = files)


#### data label TRUE ####
dl <- "mtcars data file"

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_118.dta", version = 118, data.label = dl)
save.dta13(dd, "data/dta_117.dta", version = 117, data.label = dl)
save.dta13(dd, "data/dta_115.dta", version = 115, data.label = dl)
save.dta13(dd, "data/dta_114.dta", version = 114, data.label = dl)
save.dta13(dd, "data/dta_113.dta", version = 113, data.label = dl)
save.dta13(dd, "data/dta_112.dta", version = 112, data.label = dl)
save.dta13(dd, "data/dta_111.dta", version = 111, data.label = dl)
save.dta13(dd, "data/dta_110.dta", version = 110, data.label = dl)
save.dta13(dd, "data/dta_108.dta", version = 108, data.label = dl)
save.dta13(dd, "data/dta_107.dta", version = 107, data.label = dl)
save.dta13(dd, "data/dta_106.dta", version = 106, data.label = dl)
save.dta13(dd, "data/dta_105.dta", version = 105, data.label = dl)
save.dta13(dd, "data/dta_104.dta", version = 104, data.label = dl)
save.dta13(dd, "data/dta_103.dta", version = 103, data.label = dl)
# save.dta13(dd, "data/dta_102.dta", version = 102, data.label = dl) # no data label


dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
dd106 <- read.dta13("data/dta_106.dta")
dd105 <- read.dta13("data/dta_105.dta")
dd104 <- read.dta13("data/dta_104.dta")
dd103 <- read.dta13("data/dta_103.dta")
# dd102 <- read.dta13("data/dta_102.dta")

unlink("data", recursive = TRUE)

test_that("data label", {
  # Check that rownames are identical
  expect_equal(dl, attr(dd118, "datalabel"))
  expect_equal(dl, attr(dd117, "datalabel"))
  expect_equal(dl, attr(dd115, "datalabel"))
  expect_equal(dl, attr(dd114, "datalabel"))
  expect_equal(dl, attr(dd113, "datalabel"))
  expect_equal(dl, attr(dd112, "datalabel"))
  expect_equal(dl, attr(dd111, "datalabel"))
  expect_equal(dl, attr(dd110, "datalabel"))
  expect_equal(dl, attr(dd108, "datalabel"))
  expect_equal(dl, attr(dd107, "datalabel"))
  expect_equal(dl, attr(dd106, "datalabel"))
  expect_equal(dl, attr(dd105, "datalabel"))
  expect_equal(dl, attr(dd104, "datalabel"))
  expect_equal(dl, attr(dd103, "datalabel"))
  # expect_equal(dl, attr(dd102, "datalabel"))
})

# rm(list = files)


#### convert dates TRUE ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- data.frame( dat = Sys.Date() )

save.dta13(dd, "data/dta_118.dta", version = 118, convert.dates = TRUE)
save.dta13(dd, "data/dta_117.dta", version = 117, convert.dates = TRUE)
save.dta13(dd, "data/dta_115.dta", version = 115, convert.dates = TRUE)
save.dta13(dd, "data/dta_114.dta", version = 114, convert.dates = TRUE)
save.dta13(dd, "data/dta_113.dta", version = 113, convert.dates = TRUE)
save.dta13(dd, "data/dta_112.dta", version = 112, convert.dates = TRUE)
save.dta13(dd, "data/dta_111.dta", version = 111, convert.dates = TRUE)
save.dta13(dd, "data/dta_110.dta", version = 110, convert.dates = TRUE)
save.dta13(dd, "data/dta_108.dta", version = 108, convert.dates = TRUE)
save.dta13(dd, "data/dta_107.dta", version = 107, convert.dates = TRUE)
save.dta13(dd, "data/dta_106.dta", version = 106, convert.dates = TRUE)
save.dta13(dd, "data/dta_105.dta", version = 105, convert.dates = TRUE)
save.dta13(dd, "data/dta_104.dta", version = 104, convert.dates = TRUE)
save.dta13(dd, "data/dta_103.dta", version = 103, convert.dates = TRUE)
save.dta13(dd, "data/dta_102.dta", version = 102, convert.dates = TRUE)


dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
dd106 <- read.dta13("data/dta_106.dta")
dd105 <- read.dta13("data/dta_105.dta")
dd104 <- read.dta13("data/dta_104.dta")
dd103 <- read.dta13("data/dta_103.dta")
dd102 <- read.dta13("data/dta_102.dta")

unlink("data", recursive = TRUE)

test_that("convert.dates TRUE", {
  # Check that rownames are identical
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  expect_true(datacompare(dd, dd106))
  expect_true(datacompare(dd, dd105))
  expect_true(datacompare(dd, dd104))
  expect_true(datacompare(dd, dd103))
  expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### strl save ####
if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

# strLs can be of length any length up to 2 billion characters. Starting with
# 2046 a string is handled as a strL
dd <- data.frame( dat = paste(replicate(2046, "a"), collapse = ""),
                  stringsAsFactors = FALSE)

save.dta13(dd, "data/dta_118.dta", version = 118)
save.dta13(dd, "data/dta_117.dta", version = 117)
# save.dta13(dd, "data/dta_115.dta", version = 115) # no strl
# save.dta13(dd, "data/dta_114.dta", version = 114)
# save.dta13(dd, "data/dta_113.dta", version = 113)
# save.dta13(dd, "data/dta_112.dta", version = 112)
# save.dta13(dd, "data/dta_111.dta", version = 111)
# save.dta13(dd, "data/dta_110.dta", version = 110)
# save.dta13(dd, "data/dta_108.dta", version = 108)
# save.dta13(dd, "data/dta_107.dta", version = 107)
# save.dta13(dd, "data/dta_106.dta", version = 106)
# save.dta13(dd, "data/dta_105.dta", version = 105)
# save.dta13(dd, "data/dta_104.dta", version = 104)
# save.dta13(dd, "data/dta_103.dta", version = 103)
# save.dta13(dd, "data/dta_102.dta", version = 102)


dd118 <- read.dta13("data/dta_118.dta", replace.strl = TRUE)
dd117 <- read.dta13("data/dta_117.dta", replace.strl = TRUE)
# dd115 <- read.dta13("data/dta_115.dta")
# dd114 <- read.dta13("data/dta_114.dta")
# dd113 <- read.dta13("data/dta_113.dta")
# dd112 <- read.dta13("data/dta_112.dta")
# dd111 <- read.dta13("data/dta_111.dta")
# dd110 <- read.dta13("data/dta_110.dta")
# dd108 <- read.dta13("data/dta_108.dta")
# dd107 <- read.dta13("data/dta_107.dta")
# dd106 <- read.dta13("data/dta_106.dta")
# dd105 <- read.dta13("data/dta_105.dta")
# dd104 <- read.dta13("data/dta_104.dta")
# dd103 <- read.dta13("data/dta_103.dta")
# dd102 <- read.dta13("data/dta_102.dta")

unlink("data", recursive = TRUE)

test_that("replace.strl TRUE", {
  # Check that rownames are identical
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  # expect_true(datacompare(dd, dd115))
  # expect_true(datacompare(dd, dd114))
  # expect_true(datacompare(dd, dd113))
  # expect_true(datacompare(dd, dd112))
  # expect_true(datacompare(dd, dd111))
  # expect_true(datacompare(dd, dd110))
  # expect_true(datacompare(dd, dd108))
  # expect_true(datacompare(dd, dd107))
  # expect_true(datacompare(dd, dd106))
  # expect_true(datacompare(dd, dd105))
  # expect_true(datacompare(dd, dd104))
  # expect_true(datacompare(dd, dd103))
  # expect_true(datacompare(dd, dd102))
})

# rm(list = files)

#### convert.underscore save ####
if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")


dd <- data.frame(x.1 = 1)

save.dta13(dd, "data/dta_118.dta", version = 118, convert.underscore = TRUE)
save.dta13(dd, "data/dta_117.dta", version = 117, convert.underscore = TRUE)
save.dta13(dd, "data/dta_115.dta", version = 115, convert.underscore = TRUE)
save.dta13(dd, "data/dta_114.dta", version = 114, convert.underscore = TRUE)
save.dta13(dd, "data/dta_113.dta", version = 113, convert.underscore = TRUE)
save.dta13(dd, "data/dta_112.dta", version = 112, convert.underscore = TRUE)
save.dta13(dd, "data/dta_111.dta", version = 111, convert.underscore = TRUE)
save.dta13(dd, "data/dta_110.dta", version = 110, convert.underscore = TRUE)
save.dta13(dd, "data/dta_108.dta", version = 108, convert.underscore = TRUE)
save.dta13(dd, "data/dta_107.dta", version = 107, convert.underscore = TRUE)
save.dta13(dd, "data/dta_106.dta", version = 106, convert.underscore = TRUE)
save.dta13(dd, "data/dta_105.dta", version = 105, convert.underscore = TRUE)
save.dta13(dd, "data/dta_104.dta", version = 104, convert.underscore = TRUE)
save.dta13(dd, "data/dta_103.dta", version = 103, convert.underscore = TRUE)
save.dta13(dd, "data/dta_102.dta", version = 102, convert.underscore = TRUE)


dd118 <- read.dta13("data/dta_118.dta")
dd117 <- read.dta13("data/dta_117.dta")
dd115 <- read.dta13("data/dta_115.dta")
dd114 <- read.dta13("data/dta_114.dta")
dd113 <- read.dta13("data/dta_113.dta")
dd112 <- read.dta13("data/dta_112.dta")
dd111 <- read.dta13("data/dta_111.dta")
dd110 <- read.dta13("data/dta_110.dta")
dd108 <- read.dta13("data/dta_108.dta")
dd107 <- read.dta13("data/dta_107.dta")
dd106 <- read.dta13("data/dta_106.dta")
dd105 <- read.dta13("data/dta_105.dta")
dd104 <- read.dta13("data/dta_104.dta")
dd103 <- read.dta13("data/dta_103.dta")
dd102 <- read.dta13("data/dta_102.dta")

unlink("data", recursive = TRUE)

names(dd) <- "x_1"

test_that("convert.underscore TRUE", {
  # check numerics
  expect_true(datacompare(dd, dd118))
  expect_true(datacompare(dd, dd117))
  expect_true(datacompare(dd, dd115))
  expect_true(datacompare(dd, dd114))
  expect_true(datacompare(dd, dd113))
  expect_true(datacompare(dd, dd112))
  expect_true(datacompare(dd, dd111))
  expect_true(datacompare(dd, dd110))
  expect_true(datacompare(dd, dd108))
  expect_true(datacompare(dd, dd107))
  expect_true(datacompare(dd, dd106))
  expect_true(datacompare(dd, dd105))
  expect_true(datacompare(dd, dd104))
  expect_true(datacompare(dd, dd103))
  expect_true(datacompare(dd, dd102))
  # check names
  expect_true(namescompare(dd, dd118))
  expect_true(namescompare(dd, dd117))
  expect_true(namescompare(dd, dd115))
  expect_true(namescompare(dd, dd114))
  expect_true(namescompare(dd, dd113))
  expect_true(namescompare(dd, dd112))
  expect_true(namescompare(dd, dd111))
  expect_true(namescompare(dd, dd110))
  expect_true(namescompare(dd, dd108))
  expect_true(namescompare(dd, dd107))
  expect_true(namescompare(dd, dd106))
  expect_true(namescompare(dd, dd105))
  expect_true(namescompare(dd, dd104))
  expect_true(namescompare(dd, dd103))
  expect_true(namescompare(dd, dd102))
})

# rm(list = files)

