## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.height = 4.5, fig.width = 7.5, dpi = 90, out.width = '100%')
knitr::opts_chunk$set(comment = "#>")

## ----warning = FALSE, message = FALSE-----------------------------------------
library(sfclust)
library(Matrix)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(42)
ns <- 13L; nt <- 20L

# identifiers
df <- data.frame(
  id      = seq_len(ns * nt),
  ids     = rep(seq_len(ns), nt),
  time = rep(seq_len(nt), each = ns)
)

# membership and cluster slopes
membership <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
slopes  <- c(0.10, 0.00, -0.10)

# response
df <- transform(df, y = rnorm(ns * nt, mean = slopes[membership[ids]] * time, sd = 0.4))
head(df)

## -----------------------------------------------------------------------------
# Arbitrary adjacency (upper triangle only; symmetric = TRUE mirrors it)
# Within-cluster links: {1-5}, {6-9}, {10-13}; cross-cluster links: 5-6, 9-10
i_idx <- c(1, 2, 3, 4, 1, 3,  6, 7, 8, 6,  10, 11, 12, 10,  5,  9)
j_idx <- c(2, 3, 4, 5, 5, 5,  7, 8, 9, 9,  11, 12, 13, 13,  6, 10)
adj <- sparseMatrix(i = i_idx, j = j_idx, x = 1L,
                    dims = c(ns, ns), symmetric = TRUE)

## -----------------------------------------------------------------------------
set.seed(42)
result <- sfclust(
  df, adjacency = adj, nclust = 5, fnames = "time",
  formula = y ~ f(time, model = "rw1"),
  family  = "gaussian",
  niter = 20, burnin = 0, thin = 1, nmessage = 5
)
result

## -----------------------------------------------------------------------------
summary(result, sort = TRUE)

## -----------------------------------------------------------------------------
df_fit <- fitted(result, sort = TRUE)
head(df_fit[c("id", "ids", "time", "cluster", "mean", "mean_cluster")])

## ----fig.height = 3.5---------------------------------------------------------
plot(result, sort = TRUE)

## ----fig.height = 3.5---------------------------------------------------------
plot_clusters_series(result, y, sort = TRUE) +
  facet_wrap(~ cluster, ncol = 3) +
  labs(y = "Response")

