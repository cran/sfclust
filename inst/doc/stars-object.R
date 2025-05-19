## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center", eval = TRUE)
knitr::opts_chunk$set(fig.height = 7, fig.width = 8, dpi = 150, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----warning=FALSE, include=TRUE----------------------------------------------
library(stars)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(10)
space <- st_make_grid(cellsize = c(1, 1), offset = c(0, 0), n = c(3, 2))
time <- seq(as.Date("2025-01-01"), by = "1 month", length.out = 5)

## -----------------------------------------------------------------------------
cases <- matrix(rpois(30, 100), nrow = 6, ncol = 5)
temperature <- matrix(rnorm(30), nrow = 6, ncol = 5)
precipitation <- matrix(rnorm(30), nrow = 6, ncol = 5)

## -----------------------------------------------------------------------------
stdata <- st_as_stars(
  cases = cases, temperature = temperature, precipitation = precipitation,
  dimensions = st_dimensions(geometry = space, time = time)
)
stdata

## ----fig.height = 5-----------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = cases), data = stdata) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu")

## -----------------------------------------------------------------------------
stdata["population"] <- rep(rpois(6, 1000), each = 6)
stdata

