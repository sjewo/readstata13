# readstata13

[![CRAN status](http://www.r-pkg.org/badges/version/readstata13)](https://cran.r-project.org/package=readstata13)
[![Build status](https://github.com/sjewo/readstata13/workflows/R-CMD-check/badge.svg)](https://github.com/sjewo/readstata13/actions?workflow=R-CMD-check)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/readstata13)](https://cran.r-project.org/package=readstata13)

Package to read and write all Stata file formats (version 17 and older) into a
R data.frame. The dta file format versions 102 to 119 are supported.

The function ```read.dta``` from the foreign package imports only dta files from
Stata versions <= 12. Due to the different structure and features of dta 117
files, we wrote a new file reader in Rcpp.

Additionally the package supports many features of the Stata dta format like
label sets in different languages (`?set.lang`) or business calendars
(`?as.caldays`).


## Installation

The package is hosted on CRAN.
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
To install the current release from github you need the platform specific build
tools. On Windows a current installation of
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is necessary, while OS X
users need to install
[Xcode](https://apps.apple.com/us/app/xcode/id497799835).

```R
# install.packages("devtools")
devtools::install_github("sjewo/readstata13", ref="0.10.0")
```

Older Versions of devtools require a username option:
```R
install_github("readstata13", username="sjewo", ref="0.10.0")
```

To install the current development version from github:

```R
devtools::install_github("sjewo/readstata13", ref="testing")
```


## Changelog and Features

 | Version | Changes                                                                       | 
 | ------  | ----------------------------------------------------                          | 
 | 0.10.1  | fix writing "NA", NA_character_ values                                        |
 |         | fix writing of STRLs in big endian systems                                    |
 |         |                                                                               |
 | 0.10.0  | fix for reading/writing of format 119                                         |
 |         | fix sortlist attribute for dta format 119                                     |
 |         | fix compress option. In the past, unwanted conversions to integer type could occur.|
 |         | fix encoding issues in variable and data labels                               |
 |         | fix build on FreeBSD                                                          |
 |         | new feature: improved handling of time and date formats                       |
 |         | new feature: collect warnings from read.dta13                                 |
 |         |                                                                               |
 | 0.9.2   | Fix Build on MacOS X                                                          | 
 |         |                                                                               | 
 | 0.9.1   | Allow reading only pre-selected variables                                     | 
 |         | Experimental support for format 119                                           | 
 |         | Improvements to partial reading. Idea by Kevin Jin                            | 
 |         | Export of binary data from dta-files                                          | 
 |         | new function get.label.tables() to show all Stata label sets                  | 
 |         | Fix check for duplicate labels and in set.lang()                              | 
 |         | 
 | 0.9.0   | Generate unique factor labels to prevent errors in factor definition          | 
 |         | check interrupt for long read. Patch by Giovanni Righi                        | 
 |         | Updates to notes, roxygen and register                                        | 
 |         | Fixed size of character length. Bug reported by Yiming (Paul) Li              | 
 |         | Fix saving characters containing missings. Bug reported by Eivind H. Olsen    | 
 |         | Adjustments to convert.underscore. Patch by luke-m-olson                      | 
 |         | Allow partial reading of selected rows                                        | 
 |         | 
 | 0.8.5   | Fix errors on big-endians systems                                             | 
 |         | 
 | 0.8.4   | Fix valgrind errors. converting from dta.write to writestr                    | 
 |         | Fix for empty data label                                                      | 
 |         | Make replace.strl default                                                     | 
 |         | 
 | 0.8.3   | Restrict length of varnames to 32 chars for compatibility with Stata 14       | 
 |         | Add many function tests                                                       | 
 |         | Avoid converting of double to floats while writing compressed files           | 
 |         | 
 | 0.8.2   | Save NA values in character vector as empty string                            | 
 |         | Convert.underscore=T will convert all non-literal characters to underscores   | 
 |         | Fix saving of Dates                                                           | 
 |         | Save with convert.factors by default                                          | 
 |         | Test for NaN and inf values while writing missing values and replace with NA  | 
 |         | Remove message about saving factors                                           | 
 |         | 
 | 0.8.1   | Convert non-integer variables to factors (```nonint.factors=T```)             | 
 |         | Handle large datasets                                                         | 
 |         | Working with strL variables is now a lot faster                               | 
 |         |                                                                               | 
 | <0.8.1  | Reading data files from disk or url and create a data.frame                   | 
 |         | Saving dta files to disk - most features of the dta file format are supported | 
 |         | Assign variable names                                                         | 
 |         | Read the new strL strings and save them as attribute                          | 
 |         | Convert stata label to factors and save them as attribute                     | 
 |         | Read some meta data (timestamp, dataset label, formats,...)                   | 
 |         | Convert strings to system encoding                                            | 
 |         | Handle different NA values                                                    | 
 |         | Handle multiple label languages                                               | 
 |         | Convert dates                                                                 | 
 |         | Reading business calendar files                                               | 

## readstata13 and foreign

Most attributes of the resulting data.frame are largely similar to the data.frames produced by `foreign`. 
Since newer Stata files require some additional attributes, the results of `all.equal()` and `identical()` will be `FALSE` for data.frames read by `foreign::read.dta` and `read.dta13()`.
Otherwise, the data.frames produced by both functions are identical.

```R
library(foreign)
library(readstata13)

# with factors
r12 <- read.dta("http://www.stata-press.com/data/r12/auto.dta")
r13 <- read.dta13("http://www.stata-press.com/data/r13/auto.dta")

all.equal(r12, r13, check.attributes = FALSE)

# without factors
r12 <- read.dta("http://www.stata-press.com/data/r12/auto.dta", 
                convert.factors = FALSE)
r13 <- read.dta13("http://www.stata-press.com/data/r13/auto.dta", 
                  convert.factors = FALSE)

all.equal(r12, r13, check.attributes = FALSE)
```

## Authors

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de) ([JanMarvin](https://github.com/JanMarvin)) and [Sebastian Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de) ([sjewo](https://github.com/sjewo)) 

## Licence

GPL2
