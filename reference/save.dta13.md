# Write Stata Binary Files

`save.dta13` writes a Stata dta-file bytewise and saves the data into a
dta-file.

## Usage

``` r
save.dta13(
  data,
  file,
  data.label = NULL,
  time.stamp = TRUE,
  convert.factors = TRUE,
  convert.dates = TRUE,
  tz = "GMT",
  add.rownames = FALSE,
  compress = FALSE,
  version = 117,
  convert.underscore = FALSE
)
```

## Arguments

- data:

  *data.frame.* A data.frame Object.

- file:

  *character.* Path to the dta file you want to export.

- data.label:

  *character.* Name of the dta-file.

- time.stamp:

  *logical.* If `TRUE`, add a time.stamp to the dta-file.

- convert.factors:

  *logical.* If `TRUE`, factors will be converted to Stata variables
  with labels. Stata expects strings to be encoded as Windows-1252, so
  all levels will be recoded. Character which can not be mapped in
  Windows-1252 will be saved as hexcode.

- convert.dates:

  *logical.* If `TRUE`, dates will be converted to Stata date time
  format. Code from
  [`foreign::write.dta`](https://rdrr.io/pkg/foreign/man/write.dta.html)

- tz:

  *character.* time zone specification to be used for POSIXct values and
  dates (if convert.dates is TRUE). ‘""’ is the current time zone, and
  ‘"GMT"’ is UTC (Universal Time, Coordinated).

- add.rownames:

  *logical.* If `TRUE`, a new variable rownames will be added to the
  dta-file.

- compress:

  *logical.* If `TRUE`, the resulting dta-file will use all of Statas
  numeric-vartypes.

- version:

  *numeric.* Stata format for the resulting dta-file either Stata
  version number (6 - 16) or the internal Stata dta-format (e.g. 117 for
  Stata 13). Support for large datasets: Use version="15mp" to save the
  dataset in the new Stata 15/16 MP file format. This feature is not
  thoroughly tested yet.

- convert.underscore:

  *logical.* If `TRUE`, all non numerics or non alphabet characters will
  be converted to underscores.

## Value

The function writes a dta-file to disk. The following features of the
dta file format are supported:

- datalabel::

  Dataset label

- time.stamp::

  Timestamp of file creation

- formats::

  Stata display formats. May be used with
  [`sprintf`](https://rdrr.io/r/base/sprintf.html)

- type::

  Stata data type (see Stata Corp 2014)

- var.labels::

  Variable labels

- version::

  dta file format version

- strl::

  List of character vectors for the new strL string variable type. The
  first element is the identifier and the second element the string.

## References

Stata Corp (2014): Description of .dta file format
<https://www.stata.com/help.cgi?dta>

## See also

[`read.dta`](https://rdrr.io/pkg/foreign/man/read.dta.html) in package
`foreign` and `memisc` for dta files from Stata versions \< 13 and
`read_dta` in package `haven` for Stata version \>= 13.

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
if (FALSE) { # \dontrun{
  library(readstata13)
  save.dta13(cars, file="cars.dta")
} # }
```
