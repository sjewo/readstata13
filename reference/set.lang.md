# Assign Stata Language Labels

Changes default label language for a dataset. Variables with generated
labels (option generate.labels=TRUE) are kept unchanged.

## Usage

``` r
set.lang(dat, lang = NA, generate.factors = FALSE)
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- lang:

  *character.* Label language. Default language defined by
  [`get.lang`](get.lang.md) is used if NA

- generate.factors:

  *logical.* If `TRUE`, missing factor levels are generated.

## Value

Returns a data.frame with value labels in language "lang".

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"))
get.lang(dat)
#> Available languages:
#>  en
#>  de
#> 
#> Default language:
#>  en
varlabel(dat)
#>                       id                    brand                    model 
#>             "Numeric ID"           "Brand of car"              "Car model" 
#>                     type                       hp                      max 
#>     "Car classification"            "Horse Power"          "Maximum speed" 
#>                  mileage                     ecar                    ldate 
#>                       ""                       ""            "Launch date" 
#>                 ldatecal                modelStrL 
#> "Launch date (calendar)"                       "" 

# set German label
datDE <- set.lang(dat, "de")
#> Replacing value labels. This might take some time...
#> ======================
get.lang(datDE)
#> Available languages:
#>  en
#>  de
#> 
#> Default language:
#>  de
varlabel(datDE)
#>                            id                         brand 
#>               "Numerische ID"             "Herstellermarke" 
#>                         model                          type 
#>                  "Automodell"              "Klassifikation" 
#>                            hp                           max 
#>               "Pferdestärken"       "Höchstgeschwindigkeit" 
#>                       mileage                          ecar 
#>                            ""                            "" 
#>                         ldate                      ldatecal 
#>            "Einführungsdatum" "Einführungsdatum (Kalender)" 
#>                     modelStrL 
#>                            "" 
```
