# Astronomical solution with a local cache

Returns an astronomical solution through
[`astrochron::getLaskar()`](https://rdrr.io/pkg/astrochron/man/getLaskar.html).
The download (a few MB) is kept in the user cache folder of CycloDesk,
so later calls work offline.

## Usage

``` r
cd_laskar(sol = "la04")
```

## Arguments

- sol:

  Solution name (character string): `"la04"` (eccentricity, obliquity
  and longitude of perihelion, Laskar et al. 2004), `"la10a"`,
  `"la10b"`, `"la10c"`, `"la10d"` (eccentricity, Laskar et al. 2011a) or
  `"la11"` (eccentricity, Laskar et al. 2011b). `Default="la04"`.

## Value

A data frame; the first column is time in ka.
