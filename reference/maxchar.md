# Check max char length of data.frame vectors

Stata requires us to provide the maximum size of a charactervector as
every row is stored in a bit region of this size.

## Usage

``` r
maxchar(x)
```

## Arguments

- x:

  vector of data frame

## Details

Ex: If the max chars size is four, \_ is no character in this vector: 1.
row: four 3. row: one\_ 4. row: \_\_\_\_

If a character vector contains only missings or is empty, we will assign
it a value of one, since Stata otherwise cannot handle what we write.
