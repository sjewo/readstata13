## readstata13 0.11.0 

- Initial support for Stata 18. Import .dtas files (Stata framesets) via `read.dtas()`. Alias variables are currently ignored with a warning.
- The `select.cols` argument accepts either variable names or column indices.
- Fix compilation on musl and other non-glibc based systems.
- Add package alias to readstata13.Rd

## readstata13 0.10.1 

- Fix writing `NA` and `NA_character_` values
- Fix writing of STRLs on big endian systems

## readstata13 0.10.0 

- Fix sortlist attribute for dta format 119
- Fix compress option. In the past, unwanted conversions to integer type could occur.
- Fix encoding issues in variable and data labels
- Fix for reading/writing of format 119
- Fix build on FreeBSD
- New feature: improved handling of time and date formats
- New feature: collect warnings from read.dta13

## readstata13 0.9.2 

- Fix build on OSX

## readstata13 0.9.1 

- Allow reading only pre-selected variables
- Experimental support for format 119
- Improve partial reading
- Export of binary data from dta-files
- New function get.label.tables() to show all Stata label sets
- Fix check for duplicate labels
- Fixes in set.lang

## readstata13 0.9.0 

- Generate unique factor labels to prevent errors in factor definition
- Check interrupt for long read
- Fix storage size of character vectors in save.dta13
- Fix saving characters containing missings
- Implement partial reading of dta-files
- Fix an integer bug with saving data.frames of length requiring uint64_t

## readstata13 0.8.5 

- Fix errors on big-endian systems

## readstata13 0.8.4 

- Fix valgrind errors. converting from dta.write to writestr
- Fix for empty data label
- Make replace.strl default

## readstata13 0.8.3 

- Restrict length of varnames to 32 chars for compatibility with Stata 14
- Stop compression of doubles as floats. Now test if compression of doubles as
  interger types is possible.
- Add many function tests


## readstata13 0.8.2 

- Save NA values in character vector as empty string
- Convert.underscore=T will convert all non-literal characters to underscores
- Fix saving of Dates
- Save with convert.factors by default
- Test for NaN and inf values while writing missing values and replace with NA
- Remove message about saving factors

## readstata13 0.8.1 

- Convert non-integer variables to factors (nonint.factors=T)
- Working with strL variables is now a lot faster (thank to Magnus Thor Torfason)
- Fix handling of large datasets
- Some code cleanups

## readstata13 0.8 

- Implement reading all version prior 13.
- Clean up code.
- Fix a crash when varlables do not match ncols.
- Update leap seconds R code with foreign.

## readstata13 0.7.1 

- Fix saving of files > 2GB

## readstata13 0.7 

- read and write Stata 14 files (ver 118)
- Fix save for variables without non-missing values
- Read strings from different file encodings
- Code cleanups

## readstata13 0.6.1 

- Fix heap overflow

## readstata13 0.6 

- Various fixes
- Reading stbcal-files

## readstata13 0.5-3 

- Write dta-files
- Read/write LSF and MSF files
- Source testing and cleaning
- Support for multiple label languages (see http://www.stata.com/manuals13/dlabellanguage.pdf)
- Additional tools for label handling

## readstata13 0.4 

- Convert.dates from foreign::read.dta()
- Handle different NA values
- Convert strings to system encoding
- Some checks on label assignment

## readstata13 0.3 

- Reading file from url.
  Example: `read.dta13("http://www.stata-press.com/data/r13/auto.dta")`
- Convert.underscore from foreign::read.dta(): converts _ to .
- Missing.type parts from foreign::read.dta(). If TRUE return "missing"
- New replace.strl argument to replace the reference to a STRL string in the data.frame with the actual value

## readstata13 0.2 

- Read stata characteristics and save them in extension.table attribute
- More robust handling of factor labels
- Set file encoding for all strings and convert them to system encoding
- Fixed compiler warnings

## readstata13 0.1 

- Reading data files and create a data.frame
- Assign variable names
- Read the new strL strings and save them as attribute
- Convert stata label to factors and save them as attribute
- Read some meta data (timestamp, dataset label, formats,...)

