# Pad a series at both ends

Extends an evenly spaced series by `width` (in depth or time units) at
both ends, so that sliding-window analyses (EHA, eTimeOpt, eCOCO) can
centre windows on the first and last samples and their result spans the
whole record. The added values mirror the series at its ends (without
repeating the end points) or repeat its mean. Results outside the range
of the original series should be discarded.

## Usage

``` r
cd_pad_ends(dat, width, method = c("mirror", "mean"))
```

## Arguments

- dat:

  Evenly spaced series: data frame or matrix with depth or time in
  column 1 and the values in column 2.

- width:

  Length added at each end, in the units of column 1 (numeric, \>= 0;
  usually half the window length).

- method:

  `"mirror"` (reflect the series at its ends) or `"mean"` (repeat its
  mean). `Default="mirror"`.

## Value

A data frame with two columns, the padded series. The added part is at
most one record length long at each end.

## Examples

``` r
x <- data.frame(depth = seq(0, 10, by = 0.1), value = sin(seq(0, 10, by = 0.1)))
range(cd_pad_ends(x, width = 2)[, 1])
#> [1] -2 12
```
