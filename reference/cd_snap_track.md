# Snap a tracked curve to the nearest power peaks

Completes a tracking: at every position of the scalogram the period is
set to the local maximum of power (along the period axis) that is
nearest, on a log2 scale, to the line through the tracking points.
Positions without a peak within the search width are interpolated.

## Usage

``` r
cd_snap_track(wavelet, points, tol = 0.25)
```

## Arguments

- wavelet:

  Wavelet result of
  [`WaverideR::analyse_wavelet()`](https://rdrr.io/pkg/WaverideR/man/analyse_wavelet.html)
  (or a superlet / synchrosqueezed result).

- points:

  Tracking points: data frame with depth in column 1 and period in
  column 2 (one point gives a constant guide).

- tol:

  Search width around the guide line, as a fraction of the guide period
  (numeric, \> 0). `Default=0.25`.

## Value

A data frame with columns `depth` and `period` at every position of the
scalogram.
