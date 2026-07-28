## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.height = 6.5, fig.width = 7.5, dpi = 90, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----include = FALSE----------------------------------------------------------
path_figures <- here::here(file.path("tools", "figures"))
save_figures <- dir.exists(path_figures)

## ----warning = FALSE, message = FALSE-----------------------------------------
library(sfclust)
library(stars)
library(ggplot2)
library(dplyr)
library(ggraph)

## -----------------------------------------------------------------------------
data("stgaus")
stgaus

## ----fig.height = 5-----------------------------------------------------------
stweekly <- aggregate(stgaus, by = "week", FUN = mean)
ggplot() +
    geom_stars(aes(fill = y), stweekly) +
    facet_wrap(~ time, ncol = 5) +
    scale_fill_distiller(palette = "RdBu") +
    theme_bw()

## ----fig.height = 6-----------------------------------------------------------
stgaus |>
    st_set_dimensions("geometry", values = 1:nrow(stgaus)) |>
    as_tibble() |>
    ggplot() +
    geom_line(aes(time, y, group = geometry, color = factor(geometry)), linewidth = 0.3) +
    theme_bw() +
    theme(legend.position = "none")

## -----------------------------------------------------------------------------
set.seed(123)
initial_cluster <- genclust(stgaus, nclust = 20)
names(initial_cluster)

## ----fig.height = 2.7---------------------------------------------------------
gg1 <- ggraph(initial_cluster$graph, layout = st_coordinates(st_centroid(st_geometry(stgaus)))) +
    geom_edge_fan(linetype = 1, color = 2) +
    geom_node_point(size = 1.5, color = 1) +
    geom_sf(data = st_geometry(stgaus), fill = NA, color = 1, linewidth = 0.5) +
    labs(subtitle = "(A)") +
    theme_void()
gg2 <- ggraph(initial_cluster$mst, layout = st_coordinates(st_centroid(st_geometry(stgaus)))) +
    geom_edge_fan(linetype = 1, color = 2) +
    geom_node_point(size = 1.5, color = 1) +
    geom_sf(data = st_geometry(stgaus), fill = NA, color = 1, linewidth = 0.5) +
    labs(subtitle = "(B)") +
    theme_void()
gg3 <- st_sf(st_geometry(stgaus), cluster = factor(initial_cluster$membership)) |>
  ggplot() +
    geom_sf(aes(fill = cluster), color = 1) +
    labs(subtitle = "(C)") +
    theme_void() +
    theme(legend.position = "none")
gg1 + gg2 + gg3 & theme(plot.margin = margin(0, 0, 0, 0))

## ----echo = FALSE-------------------------------------------------------------
if (save_figures) {
  ggsave(file.path(path_figures, "stgaus-initial-cluster.pdf"), width = 10, height = 3.5,
    device = cairo_pdf)
}

## ----eval = FALSE-------------------------------------------------------------
# result0 <- sfclust(stgaus, graphdata = initial_cluster, logpen = -50,
#   formula = y ~ f(id_time, model = "rw1",
#                   hyper = list(prec = list(prior = "normal", param = c(-2, 1)))),
#   niter = 50, burnin = 10, thin = 2, nmessage = 10
# )
# result0

## ----echo = FALSE-------------------------------------------------------------
result0 <- readRDS(system.file("vigdata", "gaussian-mcmc1.rds", package = "sfclust"))
result0

## ----fig.dpi = 72, fig.height = 6---------------------------------------------
plot(result0, which = 3)

## ----eval = FALSE-------------------------------------------------------------
# result <- update(result0, niter = 1000)
# result

## ----echo = FALSE-------------------------------------------------------------
result <- readRDS(system.file("vigdata", "gaussian-mcmc2.rds", package = "sfclust"))
result

## ----fig.dpi = 72, fig.height = 6---------------------------------------------
plot(result, which = 3)

## -----------------------------------------------------------------------------
gg1 <- plot(result0, which = 3) + labs(subtitle = "(A)")
gg2 <- plot(result, which = 3) + labs(subtitle = "(B)")
gg1 + gg2
if (save_figures) {
  ggsave(file.path(path_figures, "stgaus-marginal-likelihood.pdf"), width = 10, height = 4.5,
    device = cairo_pdf)
}

## -----------------------------------------------------------------------------
result_other <- update(result, sample = 750)
result_other

## -----------------------------------------------------------------------------
summary(result, sort = TRUE)

## ----fig.height = 4-----------------------------------------------------------
plot(result, which = 1:2, sort = TRUE, legend = TRUE)

## ----fig.height = 5-----------------------------------------------------------
plot_clusters_series(result, y, sort = TRUE) +
  facet_wrap(~ cluster, ncol = 5) +
  labs(title = "Risk per cluster", y = "Response")

