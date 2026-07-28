## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.height = 6.5, fig.width = 7.5, dpi = 90, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----include = FALSE----------------------------------------------------------
path_figures <- here::here(file.path("tools", "figures"))
save_figures <- dir.exists(path_figures)
save_figures <- FALSE

## ----warning = FALSE, message = FALSE-----------------------------------------
library(sfclust)
library(stars)
library(ggplot2)

## -----------------------------------------------------------------------------
stbinom

## ----fig.height = 9.5---------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = cases/population), data = stbinom) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    labs(title = "Daily risk", fill = "Risk") +
    theme_bw(base_size = 7) +
    theme(legend.position = "bottom")

## ----fig.height = 3.5---------------------------------------------------------
stweekly <- aggregate(stbinom, by = "week", FUN = mean)
ggplot() +
  geom_stars(aes(fill = cases/population), stweekly) +
  facet_wrap(~ time, nrow = 2) +
  scale_fill_distiller(palette = "RdBu") +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.93, 0.22))

## ----echo = FALSE-------------------------------------------------------------
if (save_figures) {
  ggsave(file.path(path_figures, "stbinom-weekly-risk.pdf"),
         width = 10, height = 3.5, device = cairo_pdf)
}

## -----------------------------------------------------------------------------
stbinom |>
  st_set_dimensions("geometry", values = 1:nrow(stbinom)) |>
  as.data.frame() |>
  ggplot() +
    geom_line(aes(time, cases/population, group = geometry), linewidth = 0.3) +
    theme_bw() +
    labs(title = "Risk per region", y = "Risk", x = "Time")

## -----------------------------------------------------------------------------
head(data_all(stbinom))

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

## ----fig.height = 3-----------------------------------------------------------
plot(result, sort = TRUE, legend = TRUE)

## ----echo = FALSE-------------------------------------------------------------
if (save_figures) {
  ggsave(file.path(path_figures, "stbinom-plot.pdf"), width = 10, height = 3.5,
    device = cairo_pdf)
}

## -----------------------------------------------------------------------------
summary(result)

## -----------------------------------------------------------------------------
summary(result, sample = 500)

## -----------------------------------------------------------------------------
summary(result, sort = TRUE)

## -----------------------------------------------------------------------------
pred <- fitted(result)
pred

## ----fig.height = 9.5---------------------------------------------------------
ggplot() +
    geom_stars(aes(fill = mean_inv), data = pred) +
    facet_wrap(~ time) +
    scale_fill_distiller(palette = "RdBu") +
    labs(title = "Daily risk", fill = "Risk") +
    theme_bw(base_size = 7) +
    theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
pred <- fitted(result, sort = TRUE, aggregate = TRUE)
pred

## ----fig.height = 3.5---------------------------------------------------------
predweekly <- aggregate(pred, by = "week", FUN = mean)
ggplot() +
  geom_stars(aes(fill = mean_cluster_inv), data = predweekly) +
  facet_wrap(~ time, nrow = 2) +
  scale_fill_distiller(palette = "RdBu") +
  labs(fill = "Risk") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")

## ----echo = FALSE-------------------------------------------------------------
if (save_figures) {
  ggsave(file.path(path_figures, "stbinom-weekly-risk-per-cluster.pdf"),
         width = 10, height = 3.5, device = cairo_pdf)
}

## ----fig.height = 3.5---------------------------------------------------------
plot_clusters_series(result, cases/population, sort = TRUE) +
  facet_wrap(~ cluster, ncol = 5)

## ----echo = FALSE-------------------------------------------------------------
if (save_figures) {
  ggsave(file.path(path_figures, "stbinom-risk-per-cluster.pdf"),
    width = 10, height = 4, device = cairo_pdf)
}

