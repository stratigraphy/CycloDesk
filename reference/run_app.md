# Start the CycloDesk interface

Opens the point-and-click interface in the web browser. Pick a working
folder on the left; its data files are listed below it. Select one or
more files and choose a tool from the menus; results are written as new
text files into the same folder (with Acycle-style suffixes such as
`-rsp0.02` or `-rLowess0.35`), and the equivalent R code is collected in
the R code tab.

## Usage

``` r
run_app(
  folder = NULL,
  port = NULL,
  launch.browser = TRUE,
  stop_on_close = TRUE
)
```

## Arguments

- folder:

  Working folder to start in (character string); `NULL` uses the last
  folder used, or the home folder. `Default=NULL`.

- port:

  Port number of the local web server (integer) or `NULL` for a random
  free port. `Default=NULL`.

- launch.browser:

  Open the interface in the browser (logical). `Default=TRUE`.

- stop_on_close:

  Stop R's web server when the browser tab is closed (logical).
  `Default=TRUE`.

## Value

Called for its side effect; returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
invisibly.

## Examples

``` r
if (interactive()) {
  run_app()
}
```
