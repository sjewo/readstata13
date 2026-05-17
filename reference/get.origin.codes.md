# Get Origin Code Numbers for Factors

Recreates the code numbers of a factor as stored in the Stata dataset.

## Usage

``` r
get.origin.codes(x, label.table)
```

## Arguments

- x:

  *factor.* Factor to obtain code for

- label.table:

  *table.* Table with factor levels obtained by
  [`get.label`](get.label.md).

## Value

Returns an integer with original codes

## Details

While converting numeric variables into factors, the original code
numbers are lost. This function reconstructs the codes from the
attribute `label.table`.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
labname <- get.label.name(dat,"type")
labtab <- get.label(dat, labname)

# comparsion
get.origin.codes(dat$type, labtab)
#> [1]           2           4           3           4           1           3
#> [7]  2147483620 -2147483647
as.integer(dat$type)
#> [1] 3 5 4 5 2 4 6 1
```
