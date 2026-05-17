# Get and assign Stata Variable Labels

Retrieve or set variable labels for a dataset.

## Usage

``` r
varlabel(dat, var.name = NULL, lang = NA)

varlabel(dat) <- value
```

## Arguments

- dat:

  *data.frame.* Data.frame created by `read.dta13`.

- var.name:

  *character vector.* Variable names. If NULL, get label for all
  variables.

- lang:

  *character.* Label language. Default language defined by
  [`get.lang`](get.lang.md) is used if NA

- value:

  *character vector.* Character vector of size ncol(data) with variable
  names.

## Value

Returns an named vector of variable labels

## Author

Jan Marvin Garbuszus <jan.garbuszus@ruhr-uni-bochum.de>

Sebastian Jeworutzki <sebastian.jeworutzki@ruhr-uni-bochum.de>

## Examples

``` r
dat <- read.dta13(system.file("extdata/statacar.dta", package="readstata13"),
                  convert.factors=FALSE)

# display variable labels 
varlabel(dat)
#>                       id                    brand                    model 
#>             "Numeric ID"           "Brand of car"              "Car model" 
#>                     type                       hp                      max 
#>     "Car classification"            "Horse Power"          "Maximum speed" 
#>                  mileage                     ecar                    ldate 
#>                       ""                       ""            "Launch date" 
#>                 ldatecal                modelStrL 
#> "Launch date (calendar)"                       "" 

# display german variable labels
varlabel(dat, lang="de")
#>                            id                         brand 
#>               "Numerische ID"             "Herstellermarke" 
#>                         model                          type 
#>                  "Automodell"              "Klassifikation" 
#>                            hp                           max 
#>               "Pferdestärken"       "Höchstgeschwindigkeit" 
#>                          <NA>                          <NA> 
#>                            NA                            NA 
#>                         ldate                      ldatecal 
#>            "Einführungsdatum" "Einführungsdatum (Kalender)" 
#>                          <NA> 
#>                            NA 

# display german variable label for brand
varlabel(dat, var.name = "brand", lang="de")
#>             brand 
#> "Herstellermarke" 

# define new variable labels
varlabel(dat) <- letters[1:ncol(dat)]

# display new variable labels
varlabel(dat)
#>        id     brand     model      type        hp       max   mileage      ecar 
#>       "a"       "b"       "c"       "d"       "e"       "f"       "g"       "h" 
#>     ldate  ldatecal modelStrL 
#>       "i"       "j"       "k" 
```
