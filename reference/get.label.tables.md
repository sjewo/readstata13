# Get all Stata Label Sets for a Data.frame

Retrieve the value labels for all variables.

## Usage

``` r
get.label.tables(dat)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

## Value

Returns a named list of label tables

## Details

This function returns the factor levels which represent a Stata label
set for all variables.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
get.label.tables(dat)
#> $id
#> NULL
#> 
#> $brand
#> NULL
#> 
#> $model
#> NULL
#> 
#> $type
#>         min    Off-Road    Roadster    City car  Family car         max 
#> -2147483647           1           2           3           4  2147483620 
#> 
#> $hp
#> NULL
#> 
#> $max
#> NULL
#> 
#> $mileage
#> NULL
#> 
#> $ecar
#> NULL
#> 
#> $ldate
#> NULL
#> 
#> $ldatecal
#> NULL
#> 
#> $modelStrL
#> NULL
#> 
```
