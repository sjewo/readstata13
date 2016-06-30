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
  all(unlist(Map(identical, x, y)))
}



data(mtcars)

dd <- mtcars

#### version ####

dir.create("data")

save.dta13(dd, "data/auto_118.dta", version = 118)
save.dta13(dd, "data/auto_117.dta", version = 117)
save.dta13(dd, "data/auto_115.dta", version = 115)
save.dta13(dd, "data/auto_114.dta", version = 114)
save.dta13(dd, "data/auto_113.dta", version = 113)
save.dta13(dd, "data/auto_112.dta", version = 112)
save.dta13(dd, "data/auto_111.dta", version = 111)
save.dta13(dd, "data/auto_110.dta", version = 110)
save.dta13(dd, "data/auto_108.dta", version = 108)
save.dta13(dd, "data/auto_107.dta", version = 107)
save.dta13(dd, "data/auto_106.dta", version = 106)
save.dta13(dd, "data/auto_105.dta", version = 105)
save.dta13(dd, "data/auto_104.dta", version = 104)
save.dta13(dd, "data/auto_103.dta", version = 103)
save.dta13(dd, "data/auto_102.dta", version = 102)

dd118 <- read.dta13("data/auto_118.dta")
dd117 <- read.dta13("data/auto_117.dta")
dd115 <- read.dta13("data/auto_115.dta")
dd114 <- read.dta13("data/auto_114.dta")
dd113 <- read.dta13("data/auto_113.dta")
dd112 <- read.dta13("data/auto_112.dta")
dd111 <- read.dta13("data/auto_111.dta")
dd110 <- read.dta13("data/auto_110.dta")
dd108 <- read.dta13("data/auto_108.dta")
dd107 <- read.dta13("data/auto_107.dta")
dd106 <- read.dta13("data/auto_106.dta")
dd105 <- read.dta13("data/auto_105.dta")
dd104 <- read.dta13("data/auto_104.dta")
dd103 <- read.dta13("data/auto_103.dta")
dd102 <- read.dta13("data/auto_102.dta")

# rm -r
unlink("data")

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

#### compress ####

dir.create("data")

save.dta13(dd, "data/auto_118.dta", version = 118, compress = TRUE)
save.dta13(dd, "data/auto_117.dta", version = 117, compress = TRUE)
save.dta13(dd, "data/auto_115.dta", version = 115, compress = TRUE)
save.dta13(dd, "data/auto_114.dta", version = 114, compress = TRUE)
save.dta13(dd, "data/auto_113.dta", version = 113, compress = TRUE)
save.dta13(dd, "data/auto_112.dta", version = 112, compress = TRUE)
save.dta13(dd, "data/auto_111.dta", version = 111, compress = TRUE)
save.dta13(dd, "data/auto_110.dta", version = 110, compress = TRUE)
save.dta13(dd, "data/auto_108.dta", version = 108, compress = TRUE)
save.dta13(dd, "data/auto_107.dta", version = 107, compress = TRUE)
save.dta13(dd, "data/auto_106.dta", version = 106, compress = TRUE)
save.dta13(dd, "data/auto_105.dta", version = 105, compress = TRUE)
save.dta13(dd, "data/auto_104.dta", version = 104, compress = TRUE)
save.dta13(dd, "data/auto_103.dta", version = 103, compress = TRUE)
save.dta13(dd, "data/auto_102.dta", version = 102, compress = TRUE)

dd118 <- read.dta13("data/auto_118.dta")
dd117 <- read.dta13("data/auto_117.dta")
dd115 <- read.dta13("data/auto_115.dta")
dd114 <- read.dta13("data/auto_114.dta")
dd113 <- read.dta13("data/auto_113.dta")
dd112 <- read.dta13("data/auto_112.dta")
dd111 <- read.dta13("data/auto_111.dta")
dd110 <- read.dta13("data/auto_110.dta")
dd108 <- read.dta13("data/auto_108.dta")
dd107 <- read.dta13("data/auto_107.dta")
dd106 <- read.dta13("data/auto_106.dta")
dd105 <- read.dta13("data/auto_105.dta")
dd104 <- read.dta13("data/auto_104.dta")
dd103 <- read.dta13("data/auto_103.dta")
dd102 <- read.dta13("data/auto_102.dta")

# rm -r
unlink("data")

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

#### convert.factors TRUE ####

dir.create("data")

dd <- mtcars
dd$am <- factor(x = dd$am, levels = c(0,1), labels = c("auto", "man"))


save.dta13(dd, "data/auto_118.dta", version = 118, convert.factors = TRUE)
save.dta13(dd, "data/auto_117.dta", version = 117, convert.factors = TRUE)
save.dta13(dd, "data/auto_115.dta", version = 115, convert.factors = TRUE)
save.dta13(dd, "data/auto_114.dta", version = 114, convert.factors = TRUE)
save.dta13(dd, "data/auto_113.dta", version = 113, convert.factors = TRUE)
save.dta13(dd, "data/auto_112.dta", version = 112, convert.factors = TRUE)
save.dta13(dd, "data/auto_111.dta", version = 111, convert.factors = TRUE)
save.dta13(dd, "data/auto_110.dta", version = 110, convert.factors = TRUE)
save.dta13(dd, "data/auto_108.dta", version = 108, convert.factors = TRUE)
save.dta13(dd, "data/auto_107.dta", version = 107, convert.factors = TRUE)
save.dta13(dd, "data/auto_106.dta", version = 106, convert.factors = TRUE)
save.dta13(dd, "data/auto_105.dta", version = 105, convert.factors = TRUE)
save.dta13(dd, "data/auto_104.dta", version = 104, convert.factors = TRUE)
save.dta13(dd, "data/auto_103.dta", version = 103, convert.factors = TRUE)
save.dta13(dd, "data/auto_102.dta", version = 102, convert.factors = TRUE)


dd118 <- read.dta13("data/auto_118.dta")
dd117 <- read.dta13("data/auto_117.dta")
dd115 <- read.dta13("data/auto_115.dta")
dd114 <- read.dta13("data/auto_114.dta")
dd113 <- read.dta13("data/auto_113.dta")
dd112 <- read.dta13("data/auto_112.dta")
dd111 <- read.dta13("data/auto_111.dta")
dd110 <- read.dta13("data/auto_110.dta")
dd108 <- read.dta13("data/auto_108.dta")
dd107 <- read.dta13("data/auto_107.dta")
dd106 <- read.dta13("data/auto_106.dta")
# dd105 <- read.dta13("data/auto_105.dta") no factors
# dd104 <- read.dta13("data/auto_104.dta")
# dd103 <- read.dta13("data/auto_103.dta")
# dd102 <- read.dta13("data/auto_102.dta")

# rm -r
unlink("data")

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
  expect_true(datacompare(dd, dd106))
  # expect_true(datacompare(dd, dd105)) no factors
  # expect_true(datacompare(dd, dd104))
  # expect_true(datacompare(dd, dd103))
  # expect_true(datacompare(dd, dd102))
})


#### convert.factors FALSE ####

dir.create("data")

dd <- mtcars
dd$am <- factor(x = dd$am, levels = c(0,1), labels = c("auto", "man"))


save.dta13(dd, "data/auto_118.dta", version = 118, convert.factors = FALSE)
save.dta13(dd, "data/auto_117.dta", version = 117, convert.factors = FALSE)
save.dta13(dd, "data/auto_115.dta", version = 115, convert.factors = FALSE)
save.dta13(dd, "data/auto_114.dta", version = 114, convert.factors = FALSE)
save.dta13(dd, "data/auto_113.dta", version = 113, convert.factors = FALSE)
save.dta13(dd, "data/auto_112.dta", version = 112, convert.factors = FALSE)
save.dta13(dd, "data/auto_111.dta", version = 111, convert.factors = FALSE)
save.dta13(dd, "data/auto_110.dta", version = 110, convert.factors = FALSE)
save.dta13(dd, "data/auto_108.dta", version = 108, convert.factors = FALSE)
save.dta13(dd, "data/auto_107.dta", version = 107, convert.factors = FALSE)
save.dta13(dd, "data/auto_106.dta", version = 106, convert.factors = FALSE)
save.dta13(dd, "data/auto_105.dta", version = 105, convert.factors = FALSE)
save.dta13(dd, "data/auto_104.dta", version = 104, convert.factors = FALSE)
save.dta13(dd, "data/auto_103.dta", version = 103, convert.factors = FALSE)
save.dta13(dd, "data/auto_102.dta", version = 102, convert.factors = FALSE)


dd118 <- read.dta13("data/auto_118.dta")
dd117 <- read.dta13("data/auto_117.dta")
dd115 <- read.dta13("data/auto_115.dta")
dd114 <- read.dta13("data/auto_114.dta")
dd113 <- read.dta13("data/auto_113.dta")
dd112 <- read.dta13("data/auto_112.dta")
dd111 <- read.dta13("data/auto_111.dta")
dd110 <- read.dta13("data/auto_110.dta")
dd108 <- read.dta13("data/auto_108.dta")
dd107 <- read.dta13("data/auto_107.dta")
dd106 <- read.dta13("data/auto_106.dta")
# dd105 <- read.dta13("data/auto_105.dta") no factors
# dd104 <- read.dta13("data/auto_104.dta")
# dd103 <- read.dta13("data/auto_103.dta")
# dd102 <- read.dta13("data/auto_102.dta")

dd <- mtcars
dd$am <- dd$am + 1

# rm -r
unlink("data")

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
  expect_true(datacompare(dd, dd106))
  # expect_true(datacompare(dd, dd105)) no factors
  # expect_true(datacompare(dd, dd104))
  # expect_true(datacompare(dd, dd103))
  # expect_true(datacompare(dd, dd102))
})
