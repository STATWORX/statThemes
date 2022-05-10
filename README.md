
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statThemes

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-lib/usethis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/usethis/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/statThemes)](https://CRAN.R-project.org/package=statThemes)
<!-- badges: end -->

The goal of this package is to enhance the consistency of our graphics
across the company

## Installation

You can install the development version of statThemes from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("STATWORX/statThemes")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(statThemes)
#> Lade nötiges Paket: ggplot2

col_list <- c("#0000BF", "#9BAEC1", "#9999FF")

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 2) + labs(title = "Lorem ipsum",
                              subtitle = "Consetetur Sadipscing Elitr") +
  scale_color_statworx(palette = "custom", col_list = col_list) + statworx_classic()
```

<img src="man/figures/README-example-1.png" width="100%" />

## Gallery

This gallery not only contains multiple graphics for common scenarios
but also some guidelines to improve your data visualizations. You can
find them [here](https://statworx.github.io/statThemes/).
