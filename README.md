# Lab5 R Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/732A94Labs/lab5/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/732A94Labs/lab5/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`lab5` is the Advanced R Programming Lab 5 package. It delivers a Shiny App that consumes the OpenStreetMaps API as a "Country Guessr" game. The game outputs the outline of any European country. The user then has to guess which country is displayed to advance to a new level, receiving one Score point. The game ends when the user has made 3 wrong guesses, losing all 3 health points.

## Install

```r
# install.packages("pak")
pak::pak("732A94Labs/lab5")
```

## Shiny App usage

```r
# install.packages("shiny")
shiny::runGitHub("732A94Labs/lab5_shiny")
```

The repository for the Shiny app can be found at https://github.com/732A94Labs/lab5_shiny

## Package structure

- `DESCRIPTION`, `NAMESPACE`: core package metadata and exports.
- `R/`: implementation files.
- `tests/testthat/`: automated tests
- `man/`: manual documentation for exported functions.
- `vignettes/`: starter vignette


