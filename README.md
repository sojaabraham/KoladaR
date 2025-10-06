
# Kolada API Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/sojaabraham/KoladaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sojaabraham/KoladaR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

KoladaR is an R package that provides a convenient interface to the Kolada API, allowing users to explore Key Performance Indicators (KPIs) and Municipality data for Swedish regions directly from R.

The package also includes a Shiny application that provides an interactive GUI to browse, search, and filter Kolada KPIs and municipalities.

## Installation

You can install the development version of kolada package from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("sojaabraham/KoladaR")
```

## Example
This is a basic example which shows you how this package work:

``` r
library(openKolada)

# Get all KPIs
kpis <- get_kpi()
head(kpis)

# Filter KPIs by specific IDs
selected_kpis <- get_kpi_by_Ids(c("N00951", "U00002"))
print(selected_kpis)

# Get all municipalities
muni <- get_municipality()
head(muni)

# Get municipalities by ID
get_municipality_by_Ids(c("0001", "0003"))

#Launch the shiny app
shiny::runApp("R")

```

