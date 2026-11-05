# Age model from a filtered cycle

Finds the maxima (or minima) of a filtered series and gives each one an
age one cycle period after the previous one, as in the Build Age Model
tool of Acycle (minimal tuning).

## Usage

``` r
cd_cycle_agemodel(data, cycle = 405, use = "max", pts = 3, t0 = 0)
```

## Arguments

- data:

  Data frame or matrix: depth in column 1, filtered series in column 2.

- cycle:

  Period of the filtered cycle in kyr (numeric, \> 0). `Default=405`.

- use:

  Tie points at the maxima (`"max"`) or minima (`"min"`).
  `Default="max"`.

- pts:

  Half-width of the search window in points: a sample is a maximum when
  it is the largest within `pts` samples on either side (integer, \>=
  1). `Default=3`.

- t0:

  Age of the first tie point in kyr (numeric). `Default=0`.

## Value

A data frame with columns `depth` and `age_kyr`.
