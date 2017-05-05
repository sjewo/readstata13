# readstata13

Package to read and write all Stata file formats (version 14 and older) into a
R data.frame. The dta file format versions 102 to 118 are supported.

The function ```read.dta``` from the foreign package imports only dta files from
Stata versions <= 12. Due to the different structure and features of dta 117
files, we wrote a new file reader in Rcpp.

Additionally the package supports many features of the Stata dta format like
label sets in different languages (`?set.lang`) or business calendars
(`?as.caldays`).


## Installation

The package is now hosted on CRAN.
```R
install.packages("readstata13")
```

## Usage
```R
library(readstata13)
dat <- read.dta13("path to file.dta")
save.dta13(dat, file="newfile.dta")
```

## Development Version
To install the current release from github you need the plattform specific build
tools. On Windows a current installation of
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is necessary, while OS X
users need to install
[Xcode](https://itunes.apple.com/us/app/xcode/id497799835). 

```R
# install.packages("devtools")
devtools::install_github("sjewo/readstata13", ref="0.9.0")
```

Older Versions of devtools require a username option:
```R
install_github("readstata13", username="sjewo", ref="0.9.0")
```

To install the current development version from github:

```R
devtools::install_github("sjewo/readstata13", ref="testing")
```


## Current Status

[![Build Status](https://travis-ci.org/sjewo/readstata13.svg?branch=master)](https://travis-ci.org/sjewo/readstata13)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/readstata13)](https://cran.r-project.org/package=readstata13)

### Working features
* [0.9.0] Generate unique factor labels to prevent errors in factor definition
* [0.9.0] check interrupt for long read. Patch by Giovanni Righi
* [0.9.0] updates to notes, roxygen and register
* [0.9.0] fixed size of character length. Bug reported by Yiming (Paul) Li
* [0.9.0] fix saving characters containing missings. Bug reported by Eivind H. Olsen
* [0.9.0] adjustments to convert.underscore. Patch by luke-m-olson
* [0.9.0] alow partial reading of selected rows
* [0.8.5] fix errors on big-endians systems
* [0.8.4] fix valgrind errors. converting from dta.write to writestr
* [0.8.4] fix for empty data label
* [0.8.4] make replace.strl default
* [0.8.3] restrict length of varnames to 32 chars for compatibility with Stata 14
* [0.8.3] add many function tests
* [0.8.3] avoid converting of double to floats while writing compressed files
* [0.8.2] save NA values in character vector as empty string
* [0.8.2] convert.underscore=T will convert all non-literal characters to underscores
* [0.8.2] fix saving of Dates
* [0.8.2] save with convert.factors by default
* [0.8.2] test for NaN and inf values while writing missing values and replace with NA
* [0.8.2] remove message about saving factors
* [0.8.1] convert non-integer variables to factors (```nonint.factors=T```) 
* [0.8.1] handle large datasets
* [0.8.1] working with strL variables is now a lot faster
* reading data files from disk or url and create a data.frame
* saving dta files to disk - most features of the dta file format are supported
* assign variable names
* read the new strL strings and save them as attribute 
* convert stata label to factors and save them as attribute
* read some meta data (timestamp, dataset label, formats,...)
* convert strings to system encoding
* handle different NA values
* handle multiple label languages
* convert dates
* reading business calendar files

### Todo

* cleanup of Rcpp code

### Test
Since our attributes differ from foreign::read.dta all.equal and identical
report false. If you check the values, everything is identical.

```R
library("foreign")
r12 <- read.dta("http://www.stata-press.com/data/r12/auto.dta")
r13 <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")

Map(identical,r12,r13)

att <- names(attributes(r12))
for (i in seq(att))
	cat(att[i],":", all.equal(attr(r12,att[i]),attr(r13,att[i])),"\n")

r12 <- read.dta("http://www.stata-press.com/data/r12/auto.dta",convert.factors=F)
r13 <- read.dta13("http://www.stata-press.com/data/r13/auto.dta",convert.factors=F)

Map(identical,r12,r13)
```

## Authors

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de) ([JanMarvin](https://github.com/JanMarvin)) and [Sebastian Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de) (both Ruhr-Universität Bochum)

## Licence

GPL2
