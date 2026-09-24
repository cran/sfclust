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
formula <- y ~ f(id_time, model = "rw1",
  hyper = list(prec = list(prior = "normal", param = c(-2, 1))))

## ----eval = FALSE-------------------------------------------------------------
# set.seed(123)
# result0 <- sfclust(stgaus, nclust = 20, formula = formula, logpen = -50,
#   niter = 50, burnin = 10, thin = 2, nmessage = 10,
#   path_save = "stgaus-mcmc-initial.rds")
# result0

## ----echo = FALSE-------------------------------------------------------------
result0 <- readRDS(system.file("vigdata", "gaussian-mcmc1.rds", package = "sfclust"))
result0

## ----fig.dpi = 72, fig.height = 6---------------------------------------------
plot(result0, which = 3)

## ----eval = FALSE-------------------------------------------------------------
# result <- update(result0, niter = 1000, nsave = 500, path_save = "stgaus-mcmc.rds")
# result

## ----echo = FALSE-------------------------------------------------------------
result <- readRDS(system.file("vigdata", "gaussian-mcmc2.rds", package = "sfclust"))
result

## ----fig.dpi = 72, fig.height = 6---------------------------------------------
plot(result, which = 3)

## ----fig.height = 3.5---------------------------------------------------------
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

