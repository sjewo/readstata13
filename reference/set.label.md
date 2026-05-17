# Assign Stata Labels to a Variable

Assign value labels from a Stata label set to a variable. If duplicated
labels are found, unique labels will be generated according the
following scheme: "label\_(integer code)". Levels without labels will
become \<NA\>.

## Usage

``` r
set.label(dat, var.name, lang = NA)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- var.name:

  *character.* Name of the variable in the data.frame

- lang:

  *character.* Label language. Default language defined by
  [`get.lang`](get.lang.md) is used if NA

## Value

Returns a labeled factor

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"),
                  convert.factors=FALSE)

# compare vectors
set.label(dat, "type")
#> [1] Roadster   Family car City car   Family car Off-Road   City car   max       
#> [8] min       
#> Levels: min Off-Road Roadster City car Family car max
dat$type
#> [1]           2           4           3           4           1           3
#> [7]  2147483620 -2147483647

# German label
set.label(dat, "type", "de")
#> [1] Sportwagen   Familienauto Stadtauto    Familienauto Geländewagen
#> [6] Stadtauto    max          min         
#> Levels: min Geländewagen Sportwagen Stadtauto Familienauto max
```
