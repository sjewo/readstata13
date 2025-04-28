readstata13: Read and write the ‘Stata’ file format with R
================

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/readstata13)](https://cran.r-project.org/package=readstata13)
[![Build
status](https://github.com/sjewo/readstata13/workflows/R-CMD-check/badge.svg)](https://github.com/sjewo/readstata13/actions?workflow=R-CMD-check)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/readstata13)](https://cran.r-project.org/package=readstata13)
<!-- badges: end -->

Package to read and write all Stata file formats (version 17 and older)
into a R data.frame. The dta file format versions 102 to 121 are
supported (including dtas files).

The function `read.dta` from the foreign package imports only dta files
from Stata versions \<= 12. Due to the different structure and features
of dta 117 files, we wrote a new file reader in Rcpp.

Additionally the package supports many features of the Stata dta format
like label sets in different languages (`?set.lang`) or business
calendars (`?as.caldays`).

## Installation

The package is hosted on CRAN.

``` r
install.packages("readstata13")
```

## Usage

``` r
library(readstata13)
dat <- read.dta13("path to file.dta")
save.dta13(dat, file="newfile.dta")
```

## Development Version

To install the current release from github you need the platform
specific build tools. On Windows a current installation of
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is necessary,
while OS X users need to install
[Xcode](https://apps.apple.com/us/app/xcode/id497799835).

``` r
# install.packages("remotes")
remotes::install_github("sjewo/readstata13", ref="0.11")
```

To install the current development version from github:

``` r
remotes::install_github("sjewo/readstata13", ref="testing")
```

## Changelog and Features

| Version | Changes |
|:---|:---|
| 0.11.0 | Initial support for Stata 18. Import .dtas files (Stata framesets) via `read.dtas()`. Alias variables are currently ignored with a warning. |
|  | The `select.cols` argument accepts either variable names or column indices. |
|  | Fix compilation on musl and other non-glibc based systems. |
|  | Add package alias to readstata13.Rd |

See [News](NEWS) for the full changelog.

## readstata13 and foreign

Most attributes of the resulting data.frame are largely similar to the
data.frames produced by `foreign`. Since newer Stata files require some
additional attributes, the results of `all.equal()` and `identical()`
will be `FALSE` for data.frames read by `foreign::read.dta` and
`read.dta13()`. Otherwise, the data.frames produced by both functions
are identical.

``` r
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

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de)
([JanMarvin](https://github.com/JanMarvin)) and [Sebastian
Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de)
([sjewo](https://github.com/sjewo))

## Licence

GPL2
