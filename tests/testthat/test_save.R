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

save.dta13(dd, "data/dta_15mp.dta", version = "15mp")
save.dta13(dd, "data/dta_119.dta", version = 119)
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

dd15mp<- read.dta13("data/dta_15mp.dta")
dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd15mp))
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, compress = TRUE)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, convert.factors = TRUE)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, convert.factors = FALSE)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, add.rownames = TRUE)
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

dd119 <- read.dta13("data/dta_119.dta", add.rownames = TRUE)
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
  expect_true(identical(rownames(dd), rownames(dd119)))
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
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, data.label = dl)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_equal(dl, attr(dd119, "datalabel"))
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

save.dta13(dd, "data/dta_119.dta", version = 119, convert.dates = TRUE)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd119))
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
dd <- data.frame( dat = c(paste(replicate(2046, "a"), collapse = ""),
                          paste(replicate(2046, "b"), collapse = "")),
                  stringsAsFactors = FALSE)

save.dta13(dd, "data/dta_119.dta", version = 119)
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

dd119 <- read.dta13("data/dta_119.dta", replace.strl = TRUE)
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
  expect_true(datacompare(dd, dd119))
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

save.dta13(dd, "data/dta_119.dta", version = 119, convert.underscore = TRUE)
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

dd119 <- read.dta13("data/dta_119.dta")
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
  expect_true(datacompare(dd, dd119))
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
  expect_true(namescompare(dd, dd119))
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

#### select.rows ####
if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_119.dta", version = 119)
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

dd119 <- read.dta13("data/dta_119.dta", select.rows = 5)
dd118 <- read.dta13("data/dta_118.dta", select.rows = 5)
dd117 <- read.dta13("data/dta_117.dta", select.rows = 5)
dd115 <- read.dta13("data/dta_115.dta", select.rows = 5)
dd114 <- read.dta13("data/dta_114.dta", select.rows = 5)
dd113 <- read.dta13("data/dta_113.dta", select.rows = 5)
dd112 <- read.dta13("data/dta_112.dta", select.rows = 5)
dd111 <- read.dta13("data/dta_111.dta", select.rows = 5)
dd110 <- read.dta13("data/dta_110.dta", select.rows = 5)
dd108 <- read.dta13("data/dta_108.dta", select.rows = 5)
dd107 <- read.dta13("data/dta_107.dta", select.rows = 5)
dd106 <- read.dta13("data/dta_106.dta", select.rows = 5)
dd105 <- read.dta13("data/dta_105.dta", select.rows = 5)
dd104 <- read.dta13("data/dta_104.dta", select.rows = 5)
dd103 <- read.dta13("data/dta_103.dta", select.rows = 5)
dd102 <- read.dta13("data/dta_102.dta", select.rows = 5)

unlink("data", recursive = TRUE)

dd <- dd[1:5,]

test_that("select.rows = 5", {
  # check numerics
  expect_true(datacompare(dd, dd119))
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

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_119.dta", version = 119)
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

dd119 <- read.dta13("data/dta_119.dta", select.rows = c(5,10))
dd118 <- read.dta13("data/dta_118.dta", select.rows = c(5,10))
dd117 <- read.dta13("data/dta_117.dta", select.rows = c(5,10))
dd115 <- read.dta13("data/dta_115.dta", select.rows = c(5,10))
dd114 <- read.dta13("data/dta_114.dta", select.rows = c(5,10))
dd113 <- read.dta13("data/dta_113.dta", select.rows = c(5,10))
dd112 <- read.dta13("data/dta_112.dta", select.rows = c(5,10))
dd111 <- read.dta13("data/dta_111.dta", select.rows = c(5,10))
dd110 <- read.dta13("data/dta_110.dta", select.rows = c(5,10))
dd108 <- read.dta13("data/dta_108.dta", select.rows = c(5,10))
dd107 <- read.dta13("data/dta_107.dta", select.rows = c(5,10))
dd106 <- read.dta13("data/dta_106.dta", select.rows = c(5,10))
dd105 <- read.dta13("data/dta_105.dta", select.rows = c(5,10))
dd104 <- read.dta13("data/dta_104.dta", select.rows = c(5,10))
dd103 <- read.dta13("data/dta_103.dta", select.rows = c(5,10))
dd102 <- read.dta13("data/dta_102.dta", select.rows = c(5,10))

unlink("data", recursive = TRUE)

dd <- dd[5:10,]

test_that("select.rows = c(5,10)", {
  # check numerics
  expect_true(datacompare(dd, dd119))
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

#### select.cols ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

save.dta13(dd, "data/dta_119.dta", version = 119)
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

dd119 <- read.dta13("data/dta_119.dta", select.cols = c("disp", "drat"))
dd118 <- read.dta13("data/dta_118.dta", select.cols = c("disp", "drat"))
dd117 <- read.dta13("data/dta_117.dta", select.cols = c("disp", "drat"))
dd115 <- read.dta13("data/dta_115.dta", select.cols = c("disp", "drat"))
dd114 <- read.dta13("data/dta_114.dta", select.cols = c("disp", "drat"))
dd113 <- read.dta13("data/dta_113.dta", select.cols = c("disp", "drat"))
dd112 <- read.dta13("data/dta_112.dta", select.cols = c("disp", "drat"))
dd111 <- read.dta13("data/dta_111.dta", select.cols = c("disp", "drat"))
dd110 <- read.dta13("data/dta_110.dta", select.cols = c("disp", "drat"))
dd108 <- read.dta13("data/dta_108.dta", select.cols = c("disp", "drat"))
dd107 <- read.dta13("data/dta_107.dta", select.cols = c("disp", "drat"))
dd106 <- read.dta13("data/dta_106.dta", select.cols = c("disp", "drat"))
dd105 <- read.dta13("data/dta_105.dta", select.cols = c("disp", "drat"))
dd104 <- read.dta13("data/dta_104.dta", select.cols = c("disp", "drat"))
dd103 <- read.dta13("data/dta_103.dta", select.cols = c("disp", "drat"))
dd102 <- read.dta13("data/dta_102.dta", select.cols = c("disp", "drat"))

unlink("data", recursive = TRUE)

dd <- dd[,c("disp", "drat")]

test_that("select.cols = c('disp', 'drat')", {
  # check numerics
  expect_true(datacompare(dd, dd119))
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


#### expansion.fields ####

if (readstata13:::dir.exists13("data"))
  unlink("data", recursive = TRUE)
dir.create("data")

dd <- mtcars

# create expansion.fields: In stata use command notes: They are constructed as
# follows:
#
# 1. on what is the note : can be _dta or a variable name
# 2. string "note" + number of note
# 3. the note

# initializiation of a one line note on a dta-file is done using: Ordering does
# not matter:
#
# line1: _dta note0 1
#
# line2: _dta note1 a note attached to the dta

ef <- list(
  c("_dta", "note1", "note written in R"),
  c("_dta", "note0", "1"),
  c("mpg", "note1", "Miles/(US) gallon"),
  c("mpg", "note0", "1")
)

attr(dd, "expansion.fields") <- ef

save.dta13(dd, "data/dta_119.dta", version = 119)
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
# save.dta13(dd, "data/dta_104.dta", version = 104)
# save.dta13(dd, "data/dta_103.dta", version = 103)
# save.dta13(dd, "data/dta_102.dta", version = 102)

dd119 <- attr(read.dta13("data/dta_119.dta"), "expansion.fields")
dd118 <- attr(read.dta13("data/dta_118.dta"), "expansion.fields")
dd117 <- attr(read.dta13("data/dta_117.dta"), "expansion.fields")
dd115 <- attr(read.dta13("data/dta_115.dta"), "expansion.fields")
dd114 <- attr(read.dta13("data/dta_114.dta"), "expansion.fields")
dd113 <- attr(read.dta13("data/dta_113.dta"), "expansion.fields")
dd112 <- attr(read.dta13("data/dta_112.dta"), "expansion.fields")
dd111 <- attr(read.dta13("data/dta_111.dta"), "expansion.fields")
dd110 <- attr(read.dta13("data/dta_110.dta"), "expansion.fields")
dd108 <- attr(read.dta13("data/dta_108.dta"), "expansion.fields")
dd107 <- attr(read.dta13("data/dta_107.dta"), "expansion.fields")
dd106 <- attr(read.dta13("data/dta_106.dta"), "expansion.fields")
dd105 <- attr(read.dta13("data/dta_105.dta"), "expansion.fields")
# dd104 <- read.dta13("data/dta_104.dta")
# dd103 <- read.dta13("data/dta_103.dta")
# dd102 <- read.dta13("data/dta_102.dta")

unlink("data", recursive = TRUE)

test_that("expansinon.fields", {
  # check numerics
  expect_equal(ef, dd119)
  expect_equal(ef, dd118)
  expect_equal(ef, dd117)
  expect_equal(ef, dd115)
  expect_equal(ef, dd114)
  expect_equal(ef, dd113)
  expect_equal(ef, dd112)
  expect_equal(ef, dd111)
  expect_equal(ef, dd110)
  expect_equal(ef, dd108)
  expect_equal(ef, dd107)
  expect_equal(ef, dd106)
  expect_equal(ef, dd105)
  # expect_equal(ef, dd104)
  # expect_equal(ef, dd103)
  # expect_equal(ef, dd102)
})
