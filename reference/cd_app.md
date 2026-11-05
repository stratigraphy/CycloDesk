# The CycloDesk Shiny application object

The CycloDesk Shiny application object

## Usage

``` r
cd_app(folder = NULL, stop_on_close = TRUE)
```

## Arguments

- folder:

  Working folder to start in (character string); `NULL` uses the last
  folder used, or the home folder. `Default=NULL`.

- stop_on_close:

  Stop R's web server when the browser tab is closed (logical).
  `Default=TRUE`.

## Value

A `shiny.appobj`, for example to deploy with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
