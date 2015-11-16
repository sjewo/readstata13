library(readstata13)

context("Saving datasets")

load(system.file("extdata/statacar.RData", package="readstata13"))

saveandload <- function(x, ...) {
  file <- tempfile(pattern="readstata13_", fileext=".dta")
  save.dta13(x, file=file,  ...)
  all(unlist(Map(identical, x, read.dta13(file))))
}

test_that("Saved file is identical: Version 118", {
            expect_true(saveandload(statacar, version="118", convert.factors=T))
})

