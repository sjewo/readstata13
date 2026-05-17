# Get Names of Stata Label Set

Retrieves the Stata label set in the dataset for all or an vector of
variable names.

## Usage

``` r
get.label.name(dat, var.name = NULL, lang = NA)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- var.name:

  *character vector.* Variable names. If `NULL`, get names of all label
  sets.

- lang:

  *character.* Label language. Default language defined by
  [`get.lang`](get.lang.md) is used if NA

## Value

Returns an named vector of variable labels

## Details

Stata stores factor labels in variable independent labels sets. This
function retrieves the name of the label set for a variable.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>
