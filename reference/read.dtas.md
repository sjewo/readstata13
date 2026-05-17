# Read frames from Stata dtas files

Stata 18 introduced framesets (file extension \`.dtas\`) that contain
zipped \`dta\` files. This helper functions imports those files and
returns a list of data.frames.

## Usage

``` r
read.dtas(path, select.frames = NULL, read.dta13.options = NULL)
```

## Arguments

- path:

  path to .dtas file

- select.frames:

  character vector

- read.dta13.options:

  list of parameters used in [`read.dta13`](read.dta13.md). The list
  must have the following structure:
  `list(framename = list(param = value))`

## Value

Returns a named list of data.frames.

## Examples

``` r

path <- system.file("extdata", "myproject2.dtas", package="readstata13")

# read all frames in myproject2.dtas
read.dtas(path)
#> Warning: File contains unhandled alias variable in column: 5
#> $persons
#>    personid countyid income counties median     ratio
#> 1         1        5  30818        5        0.7038001
#> 2         2        3  30752        3        0.4225046
#> 3         3        2  29673        2        0.5230381
#> 4         4        3  32115        3        0.4412310
#> 5         5        2  31189        2        0.5497603
#> 6         6        1  30992        1        0.6725256
#> 7         7        3  34328        3        0.4716356
#> 8         8        3  31508        3        0.4328914
#> 9         9        5  26071        5        0.5953915
#> 10       10        5  29768        5        0.6798210
#> 11       11        2  34757        2        0.6126525
#> 12       12        3  25630        3        0.3521330
#> 13       13        1  29146        1        0.6324675
#> 14       14        5  25752        5        0.5881063
#> 15       15        1  26806        1        0.5816895
#> 16       16        2  34368        2        0.6057957
#> 17       17        3  26914        3        0.3697740
#> 18       18        2  25886        2        0.4562857
#> 19       19        1  29321        1        0.6362650
#> 20       20        5  29571        5        0.6753220
#> 
#> $counties
#>    countyid median_income
#> 1    Brazos         46083
#> 2    Dallas         56732
#> 3    Travis         72785
#> 4    Harris         58664
#> 5    Potter         43788
#> 6   El Paso         44120
#> 7     Bowie         49153
#> 8 Galveston         69674
#> 

# read selected frames
read.dtas(path, select.frames = c("persons", "counties"))
#> Warning: File contains unhandled alias variable in column: 5
#> $persons
#>    personid countyid income counties median     ratio
#> 1         1        5  30818        5        0.7038001
#> 2         2        3  30752        3        0.4225046
#> 3         3        2  29673        2        0.5230381
#> 4         4        3  32115        3        0.4412310
#> 5         5        2  31189        2        0.5497603
#> 6         6        1  30992        1        0.6725256
#> 7         7        3  34328        3        0.4716356
#> 8         8        3  31508        3        0.4328914
#> 9         9        5  26071        5        0.5953915
#> 10       10        5  29768        5        0.6798210
#> 11       11        2  34757        2        0.6126525
#> 12       12        3  25630        3        0.3521330
#> 13       13        1  29146        1        0.6324675
#> 14       14        5  25752        5        0.5881063
#> 15       15        1  26806        1        0.5816895
#> 16       16        2  34368        2        0.6057957
#> 17       17        3  26914        3        0.3697740
#> 18       18        2  25886        2        0.4562857
#> 19       19        1  29321        1        0.6362650
#> 20       20        5  29571        5        0.6753220
#> 
#> $counties
#>    countyid median_income
#> 1    Brazos         46083
#> 2    Dallas         56732
#> 3    Travis         72785
#> 4    Harris         58664
#> 5    Potter         43788
#> 6   El Paso         44120
#> 7     Bowie         49153
#> 8 Galveston         69674
#> 

# read only frame counties
read.dtas(path, select.frames = c("counties"))
#> $counties
#>    countyid median_income
#> 1    Brazos         46083
#> 2    Dallas         56732
#> 3    Travis         72785
#> 4    Harris         58664
#> 5    Potter         43788
#> 6   El Paso         44120
#> 7     Bowie         49153
#> 8 Galveston         69674
#> 

# read frames with different arguments
read.dtas(path, 
          read.dta13.options = list(counties = list(select.cols = "median_income"),
                                     persons = list(select.cols = "income")))
#> $persons
#>    income
#> 1   30818
#> 2   30752
#> 3   29673
#> 4   32115
#> 5   31189
#> 6   30992
#> 7   34328
#> 8   31508
#> 9   26071
#> 10  29768
#> 11  34757
#> 12  25630
#> 13  29146
#> 14  25752
#> 15  26806
#> 16  34368
#> 17  26914
#> 18  25886
#> 19  29321
#> 20  29571
#> 
#> $counties
#>   median_income
#> 1         46083
#> 2         56732
#> 3         72785
#> 4         58664
#> 5         43788
#> 6         44120
#> 7         49153
#> 8         69674
#> 
```
