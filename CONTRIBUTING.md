# Contributing to CycloDesk

CycloDesk is in alpha, so the most useful contribution right now is feedback from using it on real data.

## Reporting a problem

Open a [bug report](https://github.com/stratigraphy/CycloDesk/issues/new?template=bug_report.yml) and include:

* what you did (menu, tool, parameters) and what you expected;
* the message shown in the window or in the **Log** tab;
* the code from the **R code** tab for the step that failed;
* if you can share it, a small data file that shows the problem.

Running `sessionInfo()` in R and pasting the output helps for installation problems.

## Suggesting a method or a change

Open a [feature request](https://github.com/stratigraphy/CycloDesk/issues/new?template=feature_request.yml), ideally
with a reference to the method paper or an example of where the current interface gets in the way.

## Code

Pull requests are welcome; please open an issue first for anything larger than a small fix. The calculations
belong in WaverideR (or astrochron); CycloDesk only calls them. Code style: base R, braces on every `if`/`for`
body, no apply family or tidyverse, `par()` and `options()` restored with `on.exit()`. Each menu item is one
`cd_tool()` in `R/tools*.R` with its parameters, compute, plot and generated R code; add a test in
`tests/testthat/test-tools.R` when you add a tool.
