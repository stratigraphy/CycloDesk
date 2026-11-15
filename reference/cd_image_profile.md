# Colour and brightness record along a route on an image

Samples an image along a route (a polyline of clicked points) at regular
steps. At every step the pixels on a line perpendicular to the route
(the swath) are averaged, which suppresses noise, cracks and small
clasts. The distance along the route is converted to depth (or distance)
with the scale of the image.

## Usage

``` r
cd_image_profile(
  image,
  route,
  width = 15,
  step = 1,
  scale = NA,
  start = 0,
  direction = 1,
  stat = c("mean", "median")
)
```

## Arguments

- image:

  Path of a PNG, JPEG or TIFF image, or an array as returned by
  [`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html) (values
  0-1).

- route:

  The route: data frame or matrix with the x (column, from the left) and
  y (row, from the top) pixel coordinates of at least two points, in the
  order in which the record runs.

- width:

  Width of the swath across the route in pixels (numeric, \>= 1).
  `Default=15`.

- step:

  Distance between samples along the route in pixels (numeric, \> 0).
  `Default=1`.

- scale:

  Pixels per unit of depth or distance (numeric, \> 0), e.g. from a
  scale bar; `NA` gives the distance in pixels. `Default=NA`.

- start:

  Depth or distance at the first point of the route (numeric).
  `Default=0`.

- direction:

  `1` when depth increases along the route, `-1` when it decreases (e.g.
  a route clicked upwards on a core photograph with depth increasing
  downwards). `Default=1`.

- stat:

  How the pixels of the swath are combined: `"mean"` or `"median"`
  (robust against cracks and specks). `Default="mean"`.

## Value

A data frame with one row per step: `depth`, `distance_px`, `x`, `y`
(pixel position on the route), `grey` (luma, 0-255), `L`, `a`, `b` (CIE
L\*a\*b\*), `R`, `G`, `B` (0-255), `hue` (degrees), `saturation` and
`value` (0-1). For a grey-scale image the colour columns repeat the grey
value.

## Examples

``` r
img <- array(0, dim = c(50, 200, 3))
for (j in 1:200) {
  img[, j, ] <- 0.5 + 0.4 * sin(2 * pi * j / 25)
}
prof <- cd_image_profile(img, data.frame(x = c(1, 200), y = c(25, 25)), width = 9, scale = 100)
plot(prof$depth, prof$grey, type = "l")
```
