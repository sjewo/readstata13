# readstata13: Basic Manual

The `readstata13` package was developed to address compatibility issues
arising from changes in the Stata 13 dta file format. Prior to Stata 13,
packages like `foreign` could handle dta files. However, Stata 13
introduced a new format that resembles XML.[^1] Recognizing the need for
a new solution, we (Jan Marvin Garbuszus and Sebastian Jeworutzki)
created `readstata13`. Leveraging Rcpp for performance, the package has
evolved into a comprehensive tool for working with dta files in R.

Key features of `readstata13` include:

- **Broad Format Support:** Ability to import and export dta files
  across a wide range of Stata versions, including many undocumented
  formats.
- **Handling Advanced Features:** Support for features like string
  encoding, multilingual labels, business calendars, long strings
  (`strL`), frames, and embedded binary data.
- **Enhanced Functionality:** Built as a direct replacement for
  `foreign`’s dta functions, with added capabilities for improved label
  handling (including generation) and partial data reading (selecting
  specific rows or variables).

## Core Functionality: Reading and Writing Stata files

Importing a Stata file using `readstata13` is straightforward, similar
to using the `foreign` package. The primary function is `read.dta13`. To
save an R data frame to the Stata dta format, you use the `save.dta13`
function.

``` r

data (cars)

# Save the 'cars' dataset to a Stata file
save.dta13(cars, file = "res/cars.dta")

# Read the saved Stata file back into R
dat <- read.dta13("res/cars.dta")
```

Beyond the data itself, `readstata13` preserves important metadata from
the Stata file. This information is stored as attributes of the imported
data frame.

``` r

# prints the attributes
attributes(dat)
#> $row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
#> 
#> $names
#> [1] "speed" "dist" 
#> 
#> $class
#> [1] "data.frame"
#> 
#> $datalabel
#> [1] "Written by R"
#> 
#> $time.stamp
#> [1] "17 May 2026 11:29"
#> 
#> $formats
#> [1] "%9.0g" "%9.0g"
#> 
#> $types
#> [1] 65526 65526
#> 
#> $val.labels
#>       
#> "" "" 
#> 
#> $var.labels
#> [1] "" ""
#> 
#> $version
#> [1] 117
#> 
#> $label.table
#> list()
#> 
#> $expansion.fields
#> list()
#> 
#> $byteorder
#> [1] "LSF"
#> 
#> $orig.dim
#> [1] 50  2
#> 
#> $data.label
#> character(0)
```

Examining the attributes reveals details such as the Stata format
version (e.g., format 117, introduced in Stata 13), a data label, a
timestamp, and information about the data types and formats used in
Stata. In this example, the `save.dta13` function wrote the numeric data
from R as binary `double`s in the dta file. The byte order (endianness)
is also recorded; `readstata13` is designed to handle both Little Endian
(used here) and Big Endian formats during reading and writing.[^2]

The package automatically manages the conversion of Stata’s missing
values, value labels, and variable labels during both import and export.

## Supported Stata Versions

A key advantage of `readstata13` is its ability to write dta files
compatible with older and newer versions of Stata. This is controlled
using the `version` argument in the `save.dta13` function. The table
below lists supported Stata versions and their corresponding file
formats:

| Stata Version | File Format |
|---------------|-------------|
| 18 - 19       | 121         |
| 18 - 19       | 120         |
| 15 - 19       | 119         |
| 14 - 19       | 118         |
| 13            | 117         |
| 12            | 115         |
| 10 - 11       | 114         |
| 8 - 9         | 113         |
| 7             | 110         |
| 6             | 108         |

While this table shows the most common formats, `readstata13` supports
reading files from Stata version 1 (format 102) up to the latest format
121 (used for files with over 32,767 variables, readable by Stata 18 &
19 MP).[^3] The dta format has evolved over time to accommodate larger
datasets and longer variable names or labels. Although `readstata13` can
read virtually any format, its ability to write files that *fit* within
Stata’s historical limits depends on the data size. For general
compatibility, it’s recommended to target versions 7 or later (formats
110+), which aligns with the default in
[`foreign::write.dta`](https://rdrr.io/pkg/foreign/man/write.dta.html).

Here’s an example of saving a file compatible with Stata 7:

``` r

# Save the cars dataset as a Stata 7 dta file
save.dta13(cars, "res/cars_version.dta", version = 7)

# Read the file back and check its reported version
dat3 <- read.dta13("res/cars_version.dta")
attr(dat3, "version")
#> [1] 110
```

## Working with Labelled Data

Stata datasets often include rich metadata like variable and value
labels. Since base R data frames don’t natively support this,
`readstata13` stores this information in various attributes of the
imported data frame, mirroring the approach used by
[`foreign::read.dta`](https://rdrr.io/pkg/foreign/man/read.dta.html).

Let’s use the example dataset “statacar.dta” included with the
`readstata13` package. We’ll initially import it without converting
categorical data to R factors, keeping the original numeric codes.

``` r

library(readstata13)
x <- read.dta13(system.file("extdata/statacar.dta", 
                            package = "readstata13"),
                convert.factors = FALSE)
```

Variable labels are accessible via the `var.labels` attribute:

``` r

attr(x, "var.labels")
#>  [1] "Numeric ID"             "Brand of car"           "Car model"             
#>  [4] "Car classification"     "Horse Power"            "Maximum speed"         
#>  [7] ""                       ""                       "Launch date"           
#> [10] "Launch date (calendar)" ""
```

You can retrieve the label for a specific variable using the
[`varlabel()`](../reference/varlabel.md) function:

``` r

varlabel(x, var.name = "type")
#>                 type 
#> "Car classification"
```

Value labels, which map numeric codes to descriptive text, are stored in
a more structured way. The `val.labels` attribute indicates which
variables have associated value labels. The actual label definitions
(the mapping from codes to labels) are stored as a list in the
`label.table` attribute.

In our example dataset, only one column has value labels:

``` r

attr(x, "val.labels")
#>                                 type_en                                         
#>        ""        ""        "" "type_en"        ""        ""        ""        "" 
#>                               
#>        ""        ""        ""
```

The corresponding label table for the ‘type’ variable is named
`type_en`. It’s a named vector where the numeric codes are the vector
values and the labels are the names:

``` r

attr(x, "label.table")$type_en
#>         min    Off-Road    Roadster    City car  Family car         max 
#> -2147483647           1           2           3           4  2147483620
```

Convenience functions like
[`get.label.name()`](../reference/get.label.name.md) and
[`get.label()`](../reference/get.label.md) provide alternative ways to
access this information:

``` r

get.label.name(x, var.name = "type")
#>      type 
#> "type_en"
get.label(x, "type_en")
#>         min    Off-Road    Roadster    City car  Family car         max 
#> -2147483647           1           2           3           4  2147483620
```

A common task is converting a numeric variable with value labels into an
R factor. `readstata13` simplifies this with the
[`set.label()`](../reference/set.label.md) function, which uses the
stored label information to create the factor levels.

``` r

# Create a factor variable 'type_en' from the 'type' variable using stored labels
x$type_en <- set.label(x, "type")

# Display the original numeric column and the new factor column
x[, c("type", "type_en")]
#>          type    type_en
#> 1           2   Roadster
#> 2           4 Family car
#> 3           3   City car
#> 4           4 Family car
#> 5           1   Off-Road
#> 6           3   City car
#> 7  2147483620        max
#> 8 -2147483647        min
```

### Multi-Language Support for Labels

Stata allows datasets to include labels in multiple languages.
`readstata13` supports this, and the `lang` option in
[`set.label()`](../reference/set.label.md) lets you specify which
language’s labels to use when creating a factor.

``` r

# Check available languages and the default language
get.lang(x)
#> Available languages:
#>  en
#>  de
#> 
#> Default language:
#>  en

# Create a factor using the German labels
x$type_de <- set.label(x, "type", lang = "de")

# Display the original and both language factor columns
x[, c("type", "type_en", "type_de")]
#>          type    type_en      type_de
#> 1           2   Roadster   Sportwagen
#> 2           4 Family car Familienauto
#> 3           3   City car    Stadtauto
#> 4           4 Family car Familienauto
#> 5           1   Off-Road Geländewagen
#> 6           3   City car    Stadtauto
#> 7  2147483620        max          max
#> 8 -2147483647        min          min
```

### Compatibility with Other Packages

`readstata13` is designed to integrate well with other R packages that
work with labelled data, such as `labelled` and `expss`.

``` r

# Requires labelled package version > 2.8.0 due to a past bug
library(labelled)

# Read the data and convert to the 'labelled' class format
xl <- read.dta13(system.file("extdata/statacar.dta", 
                             package = "readstata13"),
                convert.factors = FALSE)

xl <- to_labelled(xl)
xl
#> # A tibble: 8 × 11
#>      id brand   model    type     hp         max  mileage  ecar ldate ldatecal  
#> * <int> <chr>   <chr>   <int>  <int>       <dbl>    <dbl> <int> <int> <date>    
#> 1     1 Meyer   Spee…  2   e0    150    1.77e  2  1.02e 1     0     1 2001-01-03
#> 2     2 Meyer   Happ…  4   e0     98    1.45e  2  5.60e 0     0   247 2001-12-31
#> 3     3 Akiko   Susu…  3   e0     45    1.19e  2 NA           0    14 2001-01-23
#> 4     4 Akiko   Susu…  4   e0     80    1.27e  2  6.80e 0     0   134 2001-07-16
#> 5     5 Hutch   Lumb…  1   e0    180    1.56e  2  1.42e 1     0   110 2001-06-11
#> 6     6 Erikson E-Ca…  3   e0     NA   NA        NA           1   100 2001-05-25
#> 7     7 Erikson Maxi…  2.15e9  32740    8.99e307  1.70e38   100    19 2001-01-30
#> 8     7 Erikson Mimi… -2.15e9 -32767 -Inf        -1.70e38  -127     1 2001-01-03
#> # ℹ 1 more variable: modelStrL <chr>
```

Packages like `expss` can utilize the label information stored by
`readstata13` (and converted by `labelled`) for creating descriptive
tables and plots.

``` r

library(expss)
#> Loading required package: maditr
#> 
#> To select rows from data: rows(mtcars, am==0)
#> 
#> Use 'expss_output_viewer()' to display tables in the RStudio Viewer.
#>  To return to the console output, use 'expss_output_default()'.
#> 
#> Attaching package: 'expss'
#> The following object is masked from 'package:labelled':
#> 
#>     is.labelled

# Example: Use expss to create a table summarizing horse power by car brand
# First, handle missing or negative HP values
xl[xl$hp < 0 | is.na(xl$hp), "hp"] <- NA

# Create the table using expss piping syntax
xl %>%
  tab_cells(hp) %>% # Specify the variable for cells
  tab_cols(brand) %>% # Specify the variable for columns
  tab_stat_mean_sd_n() %>% # Calculate mean, standard deviation, and N
  tab_pivot() %>% # Pivot the table
  set_caption("Horse power by car brand.") # Add a caption
```

| Horse power by car brand. |                |           |         |         |
|---------------------------|----------------|-----------|---------|---------|
|                           |  Brand of car  |           |         |         |
|                           |  Akiko         |  Erikson  |  Hutch  |  Meyer  |
|  Horse Power              |                |           |         |         |
|    Mean                   | 62.5           | 32740     | 180     | 124.0   |
|    Std. dev.              | 24.7           |           |         | 36.8    |
|    Unw. valid N           | 2.0            | 1         | 1       | 2.0     |

## Handling Large Datasets

As datasets grow, importing and managing them in memory can become
challenging. `readstata13` provides features to work efficiently with
large dta files.

### Partial Reading

To avoid loading an entire large dataset when only a subset is needed,
`readstata13` allows you to read specific rows or columns. This is
particularly useful for exploring large files or extracting key
variables without consuming excessive memory or time.

``` r

# Read only the first 3 rows of the dataset
dat_1 <- read.dta13("res/cars.dta", select.rows = c(1,3)); dat_1
#>   speed dist
#> 1     4    2
#> 2     4   10
#> 3     7    4

# Read only the 'dist' variable from the dataset
dat_2 <- read.dta13("res/cars.dta", select.cols = "dist"); head(dat_2)
#>   dist
#> 1    2
#> 2   10
#> 3    4
#> 4   22
#> 5   16
#> 6   10
```

A practical application of partial reading is working with large survey
datasets like the SOEP (German Socio-Economic Panel).[^4] These datasets
are often distributed across multiple files, structured like tables in a
database. To link information across files, you need key identifier
variables. Instead of importing entire multi-gigabyte files just to get
a few ID columns, you can use `select.cols` to quickly and efficiently
read only the necessary variables.

### Compression

When saving data to a dta file, you can use the `compress = TRUE` option
in `save.dta13`. This instructs the package to use the smallest possible
Stata data type for each variable, potentially reducing the file size.

``` r

# Save the cars dataset with compression enabled
save.dta13(cars, file = "res/cars_compress.dta", compress = TRUE)

# Import the compressed file and check the resulting data types
dat2 <- read.dta13(file = "res/cars_compress.dta")
attr(dat2, "types")
#> [1] 65530 65529
```

In this example, the `numeric` vector in R was safely stored as an
`integer` in the compressed dta file because its values fit within the
integer range. The main benefit of compression is the reduction in file
size. The only notable change is that after re-import, the former
`numeric` column has become an `integer`.

``` r

rbind(file.info("res/cars.dta")["size"],
      file.info("res/cars_compress.dta")["size"])
#>                       size
#> res/cars.dta          1762
#> res/cars_compress.dta 1112
```

## Advanced Features

### Frames

Stata version 16 introduced the concept of data
[frames](https://www.stata.com/help.cgi?frames), allowing multiple
datasets to be held in memory simultaneously and saved together in a
“.dtas” file (a Stata frameset). A “.dtas” file is essentially a zip
archive containing a separate dta file for each frame.

The `get.frames` function in `readstata13` can inspect a “.dtas” file
and list the names (defined within Stata), the internal filename and
version of the frames it contains:

``` r

dtas_path <- system.file("extdata", "myproject2.dtas",
                         package="readstata13")

# Get information about frames in the .dtas file
get.frames(dtas_path)
#>       name      filename version
#> 1  persons  persons~0000     120
#> 2 counties counties~0001     118
```

To import data from a “.dtas” file, use `read.dtas`. By default, it
imports all frames and returns them as a named list of R data frames.

``` r

# Read all frames from the .dtas file
read.dtas(dtas_path)
#> Warning in stata_read(filepath, missing.type, select.rows, select.cols_chr, :
#> File contains unhandled alias variable in column: 5
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
```

You can import only specific frames using the `select.frames` argument:

``` r

# Read only the "counties" frame
read.dtas(dtas_path, select.frames = "counties")
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
```

Furthermore, you can apply specific `read.dta13` options to individual
frames within the “.dtas” file by providing a list to the
`read.dta13.options` argument. The list structure should be
`list(framename = list(param = value))`.

``` r

# Read frames with different column selections for each
read.dtas(dtas_path,
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
```

### Long Strings (strL) and Binary Data

Stata 13 introduced “long strings” (`strL`), capable of storing very
large text values. These are stored separately from the main data matrix
in the dta file, with only a reference kept in the data part.
`readstata13` handles these; by default, they are read into R character
vectors.

Interestingly, Stata also allows embedding binary data (like images,
audio, or other files) within `strL` variables.[^5] While R’s standard
data structures aren’t ideal for directly handling such embedded binary
data within a data frame,[^6] `readstata13` version `0.9.1` and later
provides the `strlexport` option to extract these binary contents to
files.

Using `strlexport = TRUE` and specifying a path with `strlpath`, you can
save the contents of `strL` variables as separate files in a designated
directory.

``` r

# Create a directory for exporting strLs
dir.create("res/strls/")

# Read a dta file containing strLs and export their content
dat_strl <- read.dta13("stata_strl.dta", 
                       strlexport = TRUE, 
                       strlpath = "res/strls/")

# List the files created in the export directory.
# The filenames indicate the variable and observation index (e.g., 15_1).
dir("res/strls/")
#> [1] "15_1" "16_1"
```

The exported files do not have extensions because the file type is not
inherently known from the `strL` data itself (and could vary cell by
cell). The user is responsible for determining the correct file type and
processing the content. In this example, the first exported file
(`15_1`) is a text file.

``` r

# Read the content of the text file strL export
readLines("res/strls/15_1")
#> [1] "R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. To download R, please choose your preferred CRAN mirror."
#> [2] ""                                                                                                                                                                                                              
#> [3] "If you have questions about R like how to download and install the software, or what the license terms are, please read our answers to frequently asked questions before you send an email."                   
#> [4] ""
```

The second file (`16_1`) is a PNG image. You can read and display it
using appropriate R packages like `png` and `grid`.

``` r

library(png)
library(grid) # grid is needed for grid.raster

# Read the PNG image file
img <- readPNG("res/strls/16_1")

# Display the image
grid::grid.raster(img)
```

![Display of the R logo extracted from a long
string.](readstata13_basic_manual_files/figure-html/unnamed-chunk-23-1.png)

[^1]: The dta format for current versions is well documented at
    <https://www.stata.com/help.cgi?dta> and also in the corresponding
    manuals.

[^2]: A detailed explanation can be found here:
    <https://en.wikipedia.org/wiki/Endianness>.

[^3]: A [development
    branch](https://github.com/sjewo/readstata13/tree/116) on GitHub
    even include support for the rarely seen `116` format, for which
    only one public sample file is known to exist.

[^4]: The SOEP is currently located at the [DIW
    Berlin](https://www.diw.de/).

[^5]: A Stata blog post illustrates this feature, showing how physicians
    could store X-ray images alongside patient data: [“In the spotlight:
    Storing long strings and entire files in Stata
    datasets”](https://www.stata.com/stata-news/news31-4/spotlight/).

[^6]: The challenge lies in R’s vector types; standard character vectors
    aren’t designed for arbitrary binary data, and there’s no native
    vector type for image processing or other binary formats within a
    data frame context. This also means `readstata13` currently cannot
    create dta files *with* embedded binary data from R.
