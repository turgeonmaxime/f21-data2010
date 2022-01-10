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


## ----echo = FALSE, warning = FALSE--------------------------------------------
library(mvtnorm)
library(purrr)
library(tidyverse)

n <- 100
mean_vec <- list(c(2, 2), c(2, -2), c(-2, -2), c(-2, 2))

dataset <- map_df(mean_vec, \(mean) {
    data <- as_tibble(rmvnorm(n, mean = mean))
    names(data) <- c("X", "Y")
    mutate(data, class = sign(prod(mean)))
})

dataset |> 
    filter(sign(X * Y) == class) |> 
    mutate(class = factor(class, levels = c(1, -1))) |> 
    ggplot(aes(x = X, y = Y, 
               colour = class)) +
    geom_point()


## -----------------------------------------------------------------------------
props <- c(13/100, 87/100)

-sum(props * log(props))

# Compare to maximum
log(2)


## -----------------------------------------------------------------------------
props <- c(13/100, 87/100)
props_m <- c(8/30, 22/30)
props_f <- c(5/70, 65/70)

entropy_full <- -sum(props * log(props))
entropy_male <- -sum(props_m * log(props_m))
entropy_fem <- -sum(props_f * log(props_f))

c(entropy_full, entropy_male, entropy_fem)


## -----------------------------------------------------------------------------
entropy_full - (29/100)*entropy_male - 
    (71/100)*entropy_fem

