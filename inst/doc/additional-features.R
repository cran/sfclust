## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.height = 7, fig.width = 8, dpi = 90, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----warning=FALSE, include=TRUE----------------------------------------------
library(sfclust)
library(stars)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
data("stgaus")
stgaus

## -----------------------------------------------------------------------------
stweekly <- aggregate(stgaus, by = "week", FUN = mean)
ggplot() +
    geom_stars(aes(fill = y), stweekly) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    theme_bw()

## ----fig.height = 7, fig.width = 8--------------------------------------------
stgaus |>
    st_set_dimensions("geometry", values = 1:nrow(stgaus)) |>
    as_tibble() |>
    ggplot() +
    geom_line(aes(time, y, group = geometry, color = factor(geometry)), linewidth = 0.3) +
    theme_bw() +
    theme(legend.position = "none")

## -----------------------------------------------------------------------------
set.seed(123)
initial_cluster <- genclust(st_geometry(stgaus), nclust = 20)
names(initial_cluster)

## -----------------------------------------------------------------------------
st_sf(st_geometry(stgaus), cluster = factor(initial_cluster$membership)) |>
  ggplot() +
    geom_sf(aes(fill = cluster)) +
    theme_bw()

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# # result <- sfclust(stgaus, graphdata = initial_cluster, niter = 50,
# #     formula = y ~ f(idt, model = "rw1", hyper = list(prec = list(prior = "normal", param = c(-2, 1)))),
# #   path_save = file.path("inst", "vigdata", "full-gaussian-mcmc1.rds"))
# # system.time(
# #   update(result, niter = 2000, path_save = file.path("inst", "vigdata", "full-gaussian-mcmc2.rds"))
# # )
# # #      user    system   elapsed
# # # 16010.960 11900.949  2794.096

## ----eval = FALSE, echo = FALSE-----------------------------------------------
# # # Reduce size of object
# # result1 <- readRDS(file.path("inst", "vigdata", "full-gaussian-mcmc1.rds"))
# # result2 <- readRDS(file.path("inst", "vigdata", "full-gaussian-mcmc2.rds"))
# # pseudo_inla <- function(x) {
# #   list(
# #     summary.random = list(id = x$summary.random$id["mean"]),
# #     summary.linear.predictor = x$summary.linear.predictor["mean"]
# #   )
# # }
# # result1$clust$models <- NULL
# # result2$clust$models <- lapply(result2$clust$models, pseudo_inla)
# # saveRDS(result1, file.path("inst", "vigdata", "gaussian-mcmc1.rds"))
# # saveRDS(result2, file.path("inst", "vigdata", "gaussian-mcmc2.rds"))

## ----eval = FALSE-------------------------------------------------------------
# result <- sfclust(stgaus, graphdata = initial_cluster, niter = 50,
#     formula = y ~ f(idt, model = "rw1", hyper = list(prec = list(prior = "normal", param = c(-2, 1)))))
# result

## ----echo = FALSE-------------------------------------------------------------
result <- readRDS(system.file("vigdata", "gaussian-mcmc1.rds", package = "sfclust"))
result

## ----fig.dpi = 72-------------------------------------------------------------
plot(result, which = 2)

## ----eval = FALSE-------------------------------------------------------------
# result2 <- update(result, niter = 2000)
# result2

## ----echo = FALSE-------------------------------------------------------------
result2 <- readRDS(system.file("vigdata", "gaussian-mcmc2.rds", package = "sfclust"))
result2

## ----fig.dpi = 72-------------------------------------------------------------
plot(result2, which = 2)

## -----------------------------------------------------------------------------
summary(result2, sort = TRUE)

## -----------------------------------------------------------------------------
stgaus$cluster <- fitted(result2, sort = TRUE)$cluster
st_sf(st_geometry(stgaus), cluster = factor(stgaus$cluster)) |>
  ggplot() +
    geom_sf(aes(fill = cluster)) +
    theme_bw()

## ----fig.height = 9.5---------------------------------------------------------
stgaus$cluster <- fitted(result2, sort = TRUE)$cluster
stgaus |>
  st_set_dimensions("geometry", values = 1:nrow(stgaus)) |>
  as_tibble() |>
  ggplot() +
    geom_line(aes(time, y, group = geometry), linewidth = 0.3) +
    facet_wrap(~ cluster, ncol = 2) +
    theme_bw() +
    labs(title = "Risk per cluster", y = "Risk", x = "Time")

