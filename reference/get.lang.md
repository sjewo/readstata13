# Show Default Label Language

Displays informations about the defined label languages.

## Usage

``` r
get.lang(dat, print = T)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- print:

  *logical.* If `TRUE`, print available languages and default language.

## Value

Returns a list with two components:

- languages::

  Vector of label languages used in the dataset

- default::

  Name of the actual default label language, otherwise NA

## Details

Stata allows to define multiple label sets in different languages. This
functions reports the available languages and the selected default
language.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>
