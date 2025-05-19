## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center", eval = TRUE)
knitr::opts_chunk$set(fig.height = 7, fig.width = 8, dpi = 90, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----warning=FALSE, include=TRUE----------------------------------------------
library(sfclust)
library(stars)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
data("stbinom")

## ----fig.height = 9.5---------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = cases/population), data = stbinom) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    labs(title = "Daily risk", fill = "Risk") +
    theme_bw(base_size = 7) +
    theme(legend.position = "bottom")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# stweekly <- aggregate(stbinom, by = "week", FUN = mean)
# ggplot() +
#   geom_stars(aes(fill = cases/population), stweekly) +
#   facet_wrap(~ time) +
#   scale_fill_distiller(palette = "RdBu") +
#   labs(title = "Weekly mean risk", fill = "Risk") +
#   theme_bw()

## -----------------------------------------------------------------------------
stbinom |>
  st_set_dimensions("geometry", values = 1:nrow(stbinom)) |>
  as_tibble() |>
  ggplot() +
    geom_line(aes(time, cases/population, group = geometry), linewidth = 0.3) +
    theme_bw() +
    labs(title = "Risk per region", y = "Risk", x = "Time")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# # set.seed(7)
# # system.time(
# #   sfclust(stbinom, formula = cases ~ poly(time, 2) + f(id),
# #     family = "binomial", Ntrials = population,
# #     niter = 2000, path_save = file.path("inst", "vigdata", "full-binomial-mcmc.rds")
# #   )
# # )
# # #      user    system   elapsed
# # # 10819.833  1417.088  1644.396

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# # # Reduce size of object
# # result <- readRDS(file.path("inst", "vigdata", "full-binomial-mcmc.rds"))
# # pseudo_inla <- function(x) {
# #   list(
# #     summary.random = list(id = x$summary.random$id["mean"]),
# #     summary.linear.predictor = x$summary.linear.predictor["mean"]
# #   )
# # }
# # result$clust$models <- lapply(result$clust$models, pseudo_inla)
# # saveRDS(result, file.path("inst", "vigdata", "binomial-mcmc.rds"))

## ----eval = FALSE-------------------------------------------------------------
# set.seed(7)
# result <- sfclust(stbinom, formula = cases ~ poly(time, 2) + f(id),
#   family = "binomial", Ntrials = population, niter = 2000)
# names(result)

## ----echo = FALSE-------------------------------------------------------------
result <- readRDS(system.file("vigdata", "binomial-mcmc.rds", package = "sfclust"))
names(result)

## -----------------------------------------------------------------------------
result

## ----fig.height = 5-----------------------------------------------------------
plot(result)

## -----------------------------------------------------------------------------
summary(result)

## -----------------------------------------------------------------------------
summary(result, sample = 500)

## -----------------------------------------------------------------------------
summary(result, sort = TRUE)

## -----------------------------------------------------------------------------
pred <- fitted(result)

## ----fig.height = 9.5---------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = mean), data = pred) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    labs(title = "Daily risk", fill = "Risk") +
    theme_bw(base_size = 7) +
    theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
pred <- fitted(result, sort = TRUE, aggregate = TRUE)

## ----fig.height = 9.5---------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = mean), data = pred) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    labs(title = "Daily risk", fill = "Risk") +
    theme_bw(base_size = 7) +
    theme(legend.position = "bottom")

## ----fig.height = 9.5---------------------------------------------------------
stbinom$cluster <- fitted(result, sort = TRUE)$cluster
stbinom |>
  st_set_dimensions("geometry", values = 1:nrow(stbinom)) |>
  as_tibble() |>
  ggplot() +
    geom_line(aes(time, cases/population, group = geometry), linewidth = 0.3) +
    facet_wrap(~ cluster, ncol = 2) +
    theme_bw() +
    labs(title = "Risk per cluster", y = "Risk", x = "Time")

