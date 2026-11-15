# Read an image as an array

Read an image as an array

## Usage

``` r
cd_image_read(file)
```

## Arguments

- file:

  Path of a PNG, JPEG or TIFF image.

## Value

A numeric array (rows x columns x channels) with values from 0 to 1: one
channel for a grey-scale image, three (red, green, blue) for a colour
image; an alpha channel is dropped.
