# Write a data series file

Writes a data frame or matrix as a tab-separated text file. Two-column
series are written without a header so that Acycle (MATLAB) can read
them; tables with more columns get a header line.

## Usage

``` r
write_series(x, file, header = NULL, digits = 10)
```

## Arguments

- x:

  Data frame or matrix with numeric columns.

- file:

  Path of the file to write (character string).

- header:

  Write the column names as the first line (logical or `NULL`; `NULL`
  writes a header only for more than two columns). `Default=NULL`.

- digits:

  Significant digits written (integer). `Default=10`.

## Value

The file path, invisibly.
