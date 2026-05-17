# List frames in Stata dtas files

Stata 18 introduced framesets (file extension \`.dtas\`) that contain
zipped \`dta\` files. This helper functions imports those files and
returns a list of data.frames.

## Usage

``` r
get.frames(path)
```

## Arguments

- path:

  path to .dtas file

## Value

Returns a data.frame with frame names, internal filenames and dta file
format version.

## Examples

``` r

path <- system.file("extdata", "myproject2.dtas", package="readstata13")

# print all frames in myproject2.dtas
get.frames(path)
#>       name      filename version
#> 1  persons  persons~0000     120
#> 2 counties counties~0001     118
```
