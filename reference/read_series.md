# Read a data series file

Reads a data file with one or more numeric columns: the first column is
depth or time, the following columns are proxy values. Supported
formats:

- text files (`.txt`, `.csv`, `.dat`, `.tsv`, `.prn`, `.asc`): the
  separator (comma, semicolon, tab or white space), a header line and a
  decimal comma (with semicolon separators) are detected automatically;
  lines starting with `#` or `%` are skipped;

- Excel workbooks (`.xlsx`, `.xls`, `.xlsm`, needs the package readxl):
  one sheet, with or without a header row;

- LAS well-log files (Log ASCII Standard 1.2 and 2.0, wrapped or not):
  the depth curve and all log curves, with the NULL value of the file
  set to `NA`.

## Usage

``` r
read_series(file, col = NULL, sheet = NULL)
```

## Arguments

- file:

  Path of the file (character string).

- col:

  Column of proxy values to return next to the first column (integer, 2
  or higher), or `NULL` to return all numeric columns. `Default=NULL`.

- sheet:

  Sheet of an Excel workbook, by name or number; `NULL` reads the first
  sheet. Ignored for other files. `Default=NULL`.

## Value

A data frame with numeric columns. The column names come from the header
(for LAS files: mnemonic and unit), or are `x`, `y`, `y2`, ... when the
file has none.

## Examples

``` r
f <- tempfile(fileext = ".txt")
write_series(data.frame(depth = 1:10, value = rnorm(10)), f)
head(read_series(f))
#>   x            y
#> 1 1 -1.400043517
#> 2 2  0.255317055
#> 3 3 -2.437263611
#> 4 4 -0.005571287
#> 5 5  0.621552721
#> 6 6  1.148411606
```
