## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

knitr::opts_chunk$set(cache = FALSE, message = FALSE,
                      linewidth = 50)


## ----simulation, echo = FALSE-------------------------------------------------
set.seed(2010)

library(tidyverse)
library(mvtnorm)
library(scales)

n <- 100
mu1 <- c(0, 0)
mu2 <- c(2, 2)

class1 <- rmvnorm(n, mean = mu1)
class2 <- rmvnorm(n, mean = mu2)

colnames(class1) <- colnames(class2) <- c("X1", "X2")

dataset <- bind_rows(
    as_tibble(class1) |> mutate(class = "positive"),
    as_tibble(class2) |> mutate(class = "negative")
)

base_plot <- ggplot(dataset, aes(X1, X2)) +
    geom_point(aes(colour = class)) +
    theme_minimal() + 
    coord_fixed()

# Record colours for future use
col_pal <- hue_pal()(2)


## ----echo = FALSE-------------------------------------------------------------
# 1. (4, 0)
new_point <- c(4, 0)

base_plot +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4)


## ----echo = FALSE-------------------------------------------------------------
nhbd_data <- dataset |> 
    mutate(dist = sqrt((X1 - new_point[1])^2 + (X2 - new_point[2])^2)) |> 
    slice_min(dist, n = 1)

base_plot +
    geom_segment(data = nhbd_data,
                 xend = new_point[1], 
                 yend = new_point[2]) +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4, 
               colour = col_pal[1])


## ----echo = FALSE-------------------------------------------------------------
# 2. (-1, 2)
new_point <- c(-1, 2)

base_plot +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4)


## ----echo = FALSE-------------------------------------------------------------
nhbd_data <- dataset |> 
    mutate(dist = sqrt((X1 - new_point[1])^2 + (X2 - new_point[2])^2)) |> 
    slice_min(dist, n = 1)

base_plot +
    geom_segment(data = nhbd_data,
                 xend = new_point[1], 
                 yend = new_point[2]) +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4, 
               colour = col_pal[2])


## ----echo = FALSE-------------------------------------------------------------
# Repeat with K=3 neighbours
nhbd_data <- dataset |> 
    mutate(dist = sqrt((X1 - new_point[1])^2 + (X2 - new_point[2])^2)) |> 
    slice_min(dist, n = 3)

base_plot +
    geom_segment(data = nhbd_data,
                 xend = new_point[1], 
                 yend = new_point[2]) +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4, 
               colour = col_pal[2]) +
    labs(title = "K=3 neighbours")


## ----echo = FALSE-------------------------------------------------------------
# Repeat with K=5 neighbours
nhbd_data <- dataset |> 
    mutate(dist = sqrt((X1 - new_point[1])^2 + (X2 - new_point[2])^2)) |> 
    slice_min(dist, n = 5)

base_plot +
    geom_segment(data = nhbd_data,
                 xend = new_point[1], 
                 yend = new_point[2]) +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4, 
               colour = col_pal[2]) +
    labs(title = "K=5 neighbours")


## ----echo = FALSE-------------------------------------------------------------
# Repeat with K=7 neighbours
nhbd_data <- dataset |> 
    mutate(dist = sqrt((X1 - new_point[1])^2 + (X2 - new_point[2])^2)) |> 
    slice_min(dist, n = 7)

base_plot +
    geom_segment(data = nhbd_data,
                 xend = new_point[1], 
                 yend = new_point[2]) +
    geom_point(x = new_point[1], 
               y = new_point[2], 
               shape = 4, 
               colour = col_pal[2]) +
    labs(title = "K=7 neighbours")


## ----echo = FALSE-------------------------------------------------------------
library(class)
new_data <- expand_grid(X1 = seq(-3, 5, length.out = 100),
                        X2 = seq(-3, 5, length.out = 100))

fit <- knn(select(dataset, -class), 
           new_data, pull(dataset, class), 
           k = 5, prob = FALSE)

new_data |> 
    mutate(class = fit) |> 
    ggplot(aes(X1, X2, colour = class)) +
    geom_point(alpha = 0.5, size = 2, 
               show.legend = FALSE, 
               shape = 15) + 
    geom_point(data = dataset, 
               aes(fill = class), 
               shape = 21, 
               colour = "black") + 
    coord_fixed(xlim = c(-2, 4),
                ylim = c(-2, 4)) +
    theme_minimal()


## ----echo = FALSE-------------------------------------------------------------
library(cowplot)

fit1 <- knn(select(dataset, -class), 
           new_data, pull(dataset, class), 
           k = 1, prob = FALSE)

fit2 <- knn(select(dataset, -class), 
           new_data, pull(dataset, class), 
           k = 9, prob = FALSE)

plot1 <- new_data |> 
    mutate(class = fit1) |> 
    ggplot(aes(X1, X2, colour = class)) +
    geom_point(alpha = 0.5, size = 2, 
               show.legend = FALSE, 
               shape = 15) + 
    geom_point(data = dataset, 
               aes(fill = class), 
               shape = 21, 
               colour = "black",
               show.legend = FALSE) + 
    coord_fixed(xlim = c(-2, 4),
                ylim = c(-2, 4)) +
    theme_minimal()

plot2 <- new_data |> 
    mutate(class = fit2) |> 
    ggplot(aes(X1, X2, colour = class)) +
    geom_point(alpha = 0.5, size = 2, 
               show.legend = FALSE, 
               shape = 15) + 
    geom_point(data = dataset, 
               aes(fill = class), 
               shape = 21, 
               colour = "black",
               show.legend = FALSE) + 
    coord_fixed(xlim = c(-2, 4),
                ylim = c(-2, 4)) +
    theme_minimal()

plot_grid(plot1, plot2, ncol = 2, 
          labels = c("K=1", "K=9"))

