# Get Stata Label Table for a Label Set

Retrieve the value labels for a specific Stata label set.

## Usage

``` r
get.label(dat, label.name)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- label.name:

  *character.* Name of the Stata label set

## Value

Returns a named vector of code numbers

## Details

This function returns the table of factor levels which represent a Stata
label set. The name of a label set for a variable can be obtained by
[`get.label.name`](get.label.name.md).

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
labname <- get.label.name(dat,"type")
get.label(dat, labname)
#>         min    Off-Road    Roadster    City car  Family car         max 
#> -2147483647           1           2           3           4  2147483620 
```
