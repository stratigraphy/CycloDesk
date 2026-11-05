# Daily mean insolation from the La2004 solution

Computes the daily mean insolation at the top of the atmosphere (Berger,
1978) for a latitude and a true solar longitude, from the eccentricity,
obliquity and longitude of perihelion of La2004.

## Usage

``` r
cd_insolation(
  tmin = 0,
  tmax = 1000,
  dt = 1,
  lat = 65,
  lon = 90,
  S0 = 1365,
  solution = NULL
)
```

## Arguments

- tmin, tmax:

  Time interval in ka (numeric, tmin \>= 0). `Default=0, 1000`.

- dt:

  Time step in ka (numeric, 1 or larger: La2004 is given every ka).
  `Default=1`.

- lat:

  Latitude in degrees (numeric, -90 to 90). `Default=65`.

- lon:

  True solar longitude in degrees from the vernal equinox (numeric): 90
  is the June solstice, 180 the September equinox, 270 the December
  solstice. `Default=90`.

- S0:

  Solar constant in W/m2 (numeric). `Default=1365`.

- solution:

  Optional data frame as returned by `cd_laskar("la04")`; `NULL` reads
  it from the cache.

## Value

A data frame with columns `time_ka` and `insolation` (W/m2).
