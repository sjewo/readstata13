library("readstata13")

dd <- read.dta13("~/Source/stata13dtas/auto.dta", convert.factors = T,
                 generate.factors = T )
dd$v2 <- c(1, rep(0, nrow(dd)-1))
save.dta13(dd, "tests/auto_118.dta", version = 118)

dd <- read.dta13("inst/extdata/statacar.dta")

dl <- "R is a programming language and software environment for statistical computing and graphics."


#### test default values ####
save.dta13(dd, "tests/auto_118.dta", version = 118)
save.dta13(dd, "tests/auto_117.dta", version = 117)
save.dta13(dd, "tests/auto_115.dta", version = 115)
save.dta13(dd, "tests/auto_114.dta", version = 114)
save.dta13(dd, "tests/auto_113.dta", version = 113)
save.dta13(dd, "tests/auto_112.dta", version = 112)
save.dta13(dd, "tests/auto_111.dta", version = 111)
save.dta13(dd, "tests/auto_110.dta", version = 110)
save.dta13(dd, "tests/auto_108.dta", version = 108)
save.dta13(dd, "tests/auto_107.dta", version = 107)
save.dta13(dd, "tests/auto_106.dta", version = 106)
save.dta13(dd, "tests/auto_105.dta", version = 105)
save.dta13(dd, "tests/auto_104.dta", version = 104)
save.dta13(dd, "tests/auto_103.dta", version = 103)
save.dta13(dd, "tests/auto_102.dta", version = 102)


dd118 <- read.dta13("tests/auto_118.dta")
dd117 <- read.dta13("tests/auto_117.dta")
dd115 <- read.dta13("tests/auto_115.dta")
dd114 <- read.dta13("tests/auto_114.dta")
dd113 <- read.dta13("tests/auto_113.dta")
dd112 <- read.dta13("tests/auto_112.dta")
dd111 <- read.dta13("tests/auto_111.dta")
dd110 <- read.dta13("tests/auto_110.dta")
dd108 <- read.dta13("tests/auto_108.dta")
dd107 <- read.dta13("tests/auto_107.dta")
dd106 <- read.dta13("tests/auto_106.dta")
dd105 <- read.dta13("tests/auto_105.dta")
dd104 <- read.dta13("tests/auto_104.dta")
dd103 <- read.dta13("tests/auto_103.dta")
dd102 <- read.dta13("tests/auto_102.dta")

files <- paste0("dd", 102:118)
files <- files[!(files %in% c("dd109","dd116"))]

for (i in files) {
  print( any(unlist(Map(identical, dd, get(i)))) )
}

#### missing.type ####
save.dta13(dd, "tests/auto_118.dta", version = 118, compress = TRUE)
save.dta13(dd, "tests/auto_117.dta", version = 117, compress = TRUE)
save.dta13(dd, "tests/auto_115.dta", version = 115, compress = TRUE)
save.dta13(dd, "tests/auto_114.dta", version = 114, compress = TRUE)
save.dta13(dd, "tests/auto_113.dta", version = 113, compress = TRUE)
save.dta13(dd, "tests/auto_112.dta", version = 112, compress = TRUE)
save.dta13(dd, "tests/auto_111.dta", version = 111, compress = TRUE)
save.dta13(dd, "tests/auto_110.dta", version = 110, compress = TRUE)
save.dta13(dd, "tests/auto_108.dta", version = 108, compress = TRUE)
save.dta13(dd, "tests/auto_107.dta", version = 107, compress = TRUE)
save.dta13(dd, "tests/auto_106.dta", version = 106, compress = TRUE)
save.dta13(dd, "tests/auto_105.dta", version = 105, compress = TRUE)
save.dta13(dd, "tests/auto_104.dta", version = 104, compress = TRUE)
save.dta13(dd, "tests/auto_103.dta", version = 103, compress = TRUE)
save.dta13(dd, "tests/auto_102.dta", version = 102, compress = TRUE)

dd118 <- read.dta13("tests/auto_118.dta", missing.type = TRUE)
dd117 <- read.dta13("tests/auto_117.dta", missing.type = TRUE)
dd115 <- read.dta13("tests/auto_115.dta", missing.type = TRUE)
dd114 <- read.dta13("tests/auto_114.dta", missing.type = TRUE)
dd113 <- read.dta13("tests/auto_113.dta", missing.type = TRUE)
dd112 <- read.dta13("tests/auto_112.dta", missing.type = TRUE)
dd111 <- read.dta13("tests/auto_111.dta", missing.type = TRUE)
dd110 <- read.dta13("tests/auto_110.dta", missing.type = TRUE)
dd108 <- read.dta13("tests/auto_108.dta", missing.type = TRUE)
dd107 <- read.dta13("tests/auto_107.dta", missing.type = TRUE)
dd106 <- read.dta13("tests/auto_106.dta", missing.type = TRUE)
dd105 <- read.dta13("tests/auto_105.dta", missing.type = TRUE)
dd104 <- read.dta13("tests/auto_104.dta", missing.type = TRUE)
dd103 <- read.dta13("tests/auto_103.dta", missing.type = TRUE)
dd102 <- read.dta13("tests/auto_102.dta", missing.type = TRUE)

for (i in files) {
  print( any(unlist(Map(identical, dd, get(i)))) )
}


dd <- read.dta13("inst/extdata/statacar.dta")
# Test it!
getRandString<-function(len=32000)
  return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),
               collapse='')
  )
txt <- paste0("a", getRandString())

dd$v2 <- c(1, 0)
dd$v2 <- factor(dd$v2, levels = c(0,1), labels = c("0", txt))

#### convert.factors
save.dta13(dd, "tests/auto_118.dta", version = 118, convert.factors = TRUE)
save.dta13(dd, "tests/auto_117.dta", version = 117, convert.factors = TRUE)
save.dta13(dd, "tests/auto_115.dta", version = 115, convert.factors = TRUE)
save.dta13(dd, "tests/auto_114.dta", version = 114, convert.factors = TRUE)
save.dta13(dd, "tests/auto_113.dta", version = 113, convert.factors = TRUE)
save.dta13(dd, "tests/auto_112.dta", version = 112, convert.factors = TRUE)
save.dta13(dd, "tests/auto_111.dta", version = 111, convert.factors = TRUE)
save.dta13(dd, "tests/auto_110.dta", version = 110, convert.factors = TRUE)
save.dta13(dd, "tests/auto_108.dta", version = 108, convert.factors = TRUE)
save.dta13(dd, "tests/auto_107.dta", version = 107, convert.factors = TRUE)
save.dta13(dd, "tests/auto_106.dta", version = 106, convert.factors = TRUE)
save.dta13(dd, "tests/auto_105.dta", version = 105, convert.factors = TRUE)
save.dta13(dd, "tests/auto_104.dta", version = 104, convert.factors = TRUE)
save.dta13(dd, "tests/auto_103.dta", version = 103, convert.factors = TRUE)
save.dta13(dd, "tests/auto_102.dta", version = 102, convert.factors = TRUE)


dd118 <- read.dta13("tests/auto_118.dta")
dd117 <- read.dta13("tests/auto_117.dta")
dd115 <- read.dta13("tests/auto_115.dta")
dd114 <- read.dta13("tests/auto_114.dta")
dd113 <- read.dta13("tests/auto_113.dta")
dd112 <- read.dta13("tests/auto_112.dta")
dd111 <- read.dta13("tests/auto_111.dta")
dd110 <- read.dta13("tests/auto_110.dta")
dd108 <- read.dta13("tests/auto_108.dta")
dd107 <- read.dta13("tests/auto_107.dta")
dd106 <- read.dta13("tests/auto_106.dta")
dd105 <- read.dta13("tests/auto_105.dta")
dd104 <- read.dta13("tests/auto_104.dta")
dd103 <- read.dta13("tests/auto_103.dta")
dd102 <- read.dta13("tests/auto_102.dta")

for (i in files) {
  print(identical(is.factor(dd$foreign), is.factor(get(i)$foreign) ))
  # print( any(unlist(Map(identical, dd118, get(i)))) )
}


#### test data label ####
save.dta13(dd, "tests/auto_118.dta", version = 118, data.label = dl)
save.dta13(dd, "tests/auto_117.dta", version = 117, data.label = dl)
save.dta13(dd, "tests/auto_115.dta", version = 115, data.label = dl)
save.dta13(dd, "tests/auto_114.dta", version = 114, data.label = dl)
save.dta13(dd, "tests/auto_113.dta", version = 113, data.label = dl)
save.dta13(dd, "tests/auto_112.dta", version = 112, data.label = dl)
save.dta13(dd, "tests/auto_111.dta", version = 111, data.label = dl)
save.dta13(dd, "tests/auto_110.dta", version = 110, data.label = dl)
save.dta13(dd, "tests/auto_108.dta", version = 108, data.label = dl)
save.dta13(dd, "tests/auto_107.dta", version = 107, data.label = dl)
save.dta13(dd, "tests/auto_106.dta", version = 106, data.label = dl)
save.dta13(dd, "tests/auto_105.dta", version = 105, data.label = dl)
save.dta13(dd, "tests/auto_104.dta", version = 104, data.label = dl)
save.dta13(dd, "tests/auto_103.dta", version = 103, data.label = dl)
save.dta13(dd, "tests/auto_102.dta", version = 102, data.label = dl)


dd118 <- read.dta13("tests/auto_118.dta")
dd117 <- read.dta13("tests/auto_117.dta")
dd115 <- read.dta13("tests/auto_115.dta")
dd114 <- read.dta13("tests/auto_114.dta")
dd113 <- read.dta13("tests/auto_113.dta")
dd112 <- read.dta13("tests/auto_112.dta")
dd111 <- read.dta13("tests/auto_111.dta")
dd110 <- read.dta13("tests/auto_110.dta")
dd108 <- read.dta13("tests/auto_108.dta")
dd107 <- read.dta13("tests/auto_107.dta")
dd106 <- read.dta13("tests/auto_106.dta")
dd105 <- read.dta13("tests/auto_105.dta")
dd104 <- read.dta13("tests/auto_104.dta")
dd103 <- read.dta13("tests/auto_103.dta")
dd102 <- read.dta13("tests/auto_102.dta")

for (i in files)
  print( attributes(get(i))$datalabel )


#### stringssize ####
dd <- read.dta13("inst/extdata/statacar.dta")

dd$string <- rep(txt, nrow(dd))
#### test data label ####
save.dta13(dd, "tests/auto_118.dta", version = 118, convert.factors = FALSE)
save.dta13(dd, "tests/auto_117.dta", version = 117, convert.factors = FALSE)
save.dta13(dd, "tests/auto_115.dta", version = 115, convert.factors = FALSE)
save.dta13(dd, "tests/auto_114.dta", version = 114, convert.factors = FALSE)
save.dta13(dd, "tests/auto_113.dta", version = 113, convert.factors = FALSE)
save.dta13(dd, "tests/auto_112.dta", version = 112, convert.factors = FALSE)
save.dta13(dd, "tests/auto_111.dta", version = 111, convert.factors = FALSE)
save.dta13(dd, "tests/auto_110.dta", version = 110, convert.factors = FALSE)
save.dta13(dd, "tests/auto_108.dta", version = 108, convert.factors = FALSE)
save.dta13(dd, "tests/auto_107.dta", version = 107, convert.factors = FALSE)
save.dta13(dd, "tests/auto_106.dta", version = 106, convert.factors = FALSE)
save.dta13(dd, "tests/auto_105.dta", version = 105, convert.factors = FALSE)
save.dta13(dd, "tests/auto_104.dta", version = 104, convert.factors = FALSE)
save.dta13(dd, "tests/auto_103.dta", version = 103, convert.factors = FALSE)
save.dta13(dd, "tests/auto_102.dta", version = 102, convert.factors = FALSE)


dd118 <- read.dta13("tests/auto_118.dta")
dd117 <- read.dta13("tests/auto_117.dta")
dd115 <- read.dta13("tests/auto_115.dta")
dd114 <- read.dta13("tests/auto_114.dta")
dd113 <- read.dta13("tests/auto_113.dta")
dd112 <- read.dta13("tests/auto_112.dta")
dd111 <- read.dta13("tests/auto_111.dta")
dd110 <- read.dta13("tests/auto_110.dta")
dd108 <- read.dta13("tests/auto_108.dta")
dd107 <- read.dta13("tests/auto_107.dta")
dd106 <- read.dta13("tests/auto_106.dta")
dd105 <- read.dta13("tests/auto_105.dta")
dd104 <- read.dta13("tests/auto_104.dta")
dd103 <- read.dta13("tests/auto_103.dta")
dd102 <- read.dta13("tests/auto_102.dta")
