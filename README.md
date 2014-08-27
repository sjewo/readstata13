#readstata13

Package to read the new Stata 13 file format (dta version 117) into a R data.frame. 

The function ```read.dta``` from the foreign package imports only dta files from Stata versions <= 12. Due to the different structure and features of dta 117 files, we wrote a new file reader in Rccp.


## Installation

To install the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("sjewo/readstata13")
```
## Current Status

[![Build Status](https://travis-ci.org/sjewo/readstata13.png)](https://travis-ci.org/sjewo/readstata13)

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

[Marvin Garbuszus](mailto:jan.garbuszus@ruhr-uni-bochum.de) and [Sebastian Jeworutzki](mailto:Sebastian.Jeworutzki@ruhr-uni-bochum.de) (both Ruhr-Universität Bochum)

## Licence

GPL2
