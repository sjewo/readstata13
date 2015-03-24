#readstata13

Package to read and write the new Stata 13 file format (dta version 117) into a R data.frame. 

The function ```read.dta``` from the foreign package imports only dta files from Stata versions <= 12. Due to the different structure and features of dta 117 files, we wrote a new file reader in Rccp.

Additionally the package supports many features of the Stata dta format like label sets in different languages (`?set.lang`) or business calendars (`?as.caldays`).


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
To install the current release (0.6.1) from github you need the plattform specific build tools. On Windows a current installation of [Rtools](http://cran.r-project.org/bin/windows/Rtools/) is necessary, while OS X users need to install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835). 

```R
# install.packages("devtools")
devtools::install_github("sjewo/readstata13", ref="0.6.1")
```

Older Versions of devtools require a username option:
```R
install_github("readstata13", username="sjewo", ref="0.6.1")
```

To install the current development version from github:

```R
devtools::install_github("sjewo/readstata13", ref="testing")
```


## Current Status

[![Build Status](https://travis-ci.org/sjewo/readstata13.svg?branch=master)](https://travis-ci.org/sjewo/readstata13)

### Working features

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

* cleanup of Rccp code

### Test
Since our attributes differ from foreign::read.dta all.equal and identical report false. If you check the values, everything is identical.

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
