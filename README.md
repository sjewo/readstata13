# readstata13

Package to read and write all Stata file formats (version 15 and older) into a
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
devtools::install_github("sjewo/readstata13", ref="0.9.2")
```

Older Versions of devtools require a username option:
```R
install_github("readstata13", username="sjewo", ref="0.9.2")
```

To install the current development version from github:

```R
devtools::install_github("sjewo/readstata13", ref="testing")
```


## Current Status

[![Build Status](https://travis-ci.org/sjewo/readstata13.svg?branch=master)](https://travis-ci.org/sjewo/readstata13)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/readstata13)](https://cran.r-project.org/package=readstata13)

### Changelog and Features

 | Version | Changes                                                                       | 
 | ------  | ----------------------------------------------------                          | 
 | 0.9.2   | Fix Build on MacOS X                                                          | 
 |         |                                                                               | 
 | 0.9.1   | Allow reading only pre-selected variables                                     | 
 | 0.9.1   | Experimental support for format 119                                           | 
 | 0.9.1   | Improvements to partial reading. Idea by Kevin Jin                            | 
 | 0.9.1   | Export of binary data from dta-files                                          | 
 | 0.9.1   | new function get.label.tables() to show all Stata label sets                  | 
 | 0.9.1   | Fix check for duplicate labels and in set.lang()                              | 
 |         | 
 | 0.9.0   | Generate unique factor labels to prevent errors in factor definition          | 
 | 0.9.0   | check interrupt for long read. Patch by Giovanni Righi                        | 
 | 0.9.0   | Updates to notes, roxygen and register                                        | 
 | 0.9.0   | Fixed size of character length. Bug reported by Yiming (Paul) Li              | 
 | 0.9.0   | Fix saving characters containing missings. Bug reported by Eivind H. Olsen    | 
 | 0.9.0   | Adjustments to convert.underscore. Patch by luke-m-olson                      | 
 | 0.9.0   | Allow partial reading of selected rows                                        | 
 |         | 
 | 0.8.5   | Fix errors on big-endians systems                                             | 
 |         | 
 | 0.8.4   | Fix valgrind errors. converting from dta.write to writestr                    | 
 | 0.8.4   | Fix for empty data label                                                      | 
 | 0.8.4   | Make replace.strl default                                                     | 
 |         | 
 | 0.8.3   | Restrict length of varnames to 32 chars for compatibility with Stata 14       | 
 | 0.8.3   | Add many function tests                                                       | 
 | 0.8.3   | Avoid converting of double to floats while writing compressed files           | 
 |         | 
 | 0.8.2   | Save NA values in character vector as empty string                            | 
 | 0.8.2   | Convert.underscore=T will convert all non-literal characters to underscores   | 
 | 0.8.2   | Fix saving of Dates                                                           | 
 | 0.8.2   | Save with convert.factors by default                                          | 
 | 0.8.2   | Test for NaN and inf values while writing missing values and replace with NA  | 
 | 0.8.2   | Remove message about saving factors                                           | 
 |         | 
 | 0.8.1   | Convert non-integer variables to factors (```nonint.factors=T```)             | 
 | 0.8.1   | Handle large datasets                                                         | 
 | 0.8.1   | Working with strL variables is now a lot faster                               | 
 |         |                                                                               | 
 | <0.8.1  | Reading data files from disk or url and create a data.frame                   | 
 | <0.8.1  | Saving dta files to disk - most features of the dta file format are supported | 
 | <0.8.1  | Assign variable names                                                         | 
 | <0.8.1  | Read the new strL strings and save them as attribute                          | 
 | <0.8.1  | Convert stata label to factors and save them as attribute                     | 
 | <0.8.1  | Read some meta data (timestamp, dataset label, formats,...)                   | 
 | <0.8.1  | Convert strings to system encoding                                            | 
 | <0.8.1  | Handle different NA values                                                    | 
 | <0.8.1  | Handle multiple label languages                                               | 
 | <0.8.1  | Convert dates                                                                 | 
 | <0.8.1  | Reading business calendar files                                               | 

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

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de) ([JanMarvin](https://github.com/JanMarvin)) and [Sebastian Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de) ([sjewo](https://github.com/sjewo)) 

## Licence

GPL2
