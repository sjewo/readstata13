#readstata13

Package to read the new Stata 13 file format (dta version 117) into a R data.frame. 

The function ```read.dta``` from the foreign package imports only dta files from Stata versions <= 12. Due to the different structure and features of dta 117 files, we wrote a new file reader in Rccp.


## Installation

To install the current release (0.1) from github:

```R
# install.packages("devtools")
devtools::install_github("sjewo/readstata13", ref="0.1")
```

Older Versions of devtools require a username option:
```R
install_github("readstata13", username="sjewo", ref="0.1")
```

To install the current development version from github:

```R
devtools::install_github("sjewo/readstata13")
```

## Usage
```R
library(readstata13)
dat <- read.dta13("path to file.dta")
```

## Current Status

[![Build Status](https://travis-ci.org/sjewo/readstata13.svg?branch=master)](https://travis-ci.org/sjewo/readstata13)

### Working features

* reading data files and create a data.frame
* assign variable names
* read the new strL strings and save them as attribute
* convert stata label to factors and save them as attribute
* read some meta data (timestamp, dataset label, formats,...)

### Todo

* convert dates
* handle different NA values
* character encoding
* add test datasets
* write stata 13 dta files
* cleanup of Rccp code

## Authors

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de) ([JanMarvin](https://github.com/JanMarvin)) and [Sebastian Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de) (both Ruhr-Universität Bochum)

## Licence

GPL2
