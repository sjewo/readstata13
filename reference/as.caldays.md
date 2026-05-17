# Convert Stata business calendar dates in readable dates.

Convert Stata business calendar dates in readable dates.

## Usage

``` r
as.caldays(buisdays, cal, format = "%Y-%m-%d")
```

## Arguments

- buisdays:

  numeric Vector of business dates

- cal:

  data.frame Conversion table for business calendar dates

- format:

  character String with date format as in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html)

## Value

Returns a vector of readable dates.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
# read business calendar and data
sp500 <- stbcal(system.file("extdata/sp500.stbcal", package="readstata13"))
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))

# convert dates and check
dat$ldatescal2 <- as.caldays(dat$ldate, sp500)
all(dat$ldatescal2==dat$ldatescal)
#> [1] TRUE
```
