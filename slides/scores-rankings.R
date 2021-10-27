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


## -----------------------------------------------------------------------------
library(tidyverse)

data_fev <- read_csv("FEV.csv")


## -----------------------------------------------------------------------------
# FEV vs Age
data_fev |> 
  ggplot(aes(x = age, y = fev)) +
  geom_point()


## -----------------------------------------------------------------------------
# FEV vs height
data_fev |> 
  ggplot(aes(x = height, y = fev)) +
  geom_point()


## -----------------------------------------------------------------------------
# Create new FEV score
data_fev <- data_fev |> 
  mutate(fev2 = fev/height^2)

# FEV2 vs Age
data_fev |> 
  ggplot(aes(x = age, y = fev2)) +
  geom_point()


## -----------------------------------------------------------------------------
data_fev |> 
  summarise(cor(age, fev), cor(age, fev2))


## ---- echo = FALSE------------------------------------------------------------
library(tidyverse)
library(cowplot)

data.frame(
  diff = seq(-400, 400, length.out = 100)
) |> 
  mutate(prob = (1 + 10^(-diff/400))^-1) |> 
  ggplot(aes(x = diff, y = prob)) + 
  geom_line() + 
  theme_minimal_grid() +
  xlab(expression(r(A) - r(B))) +
  ylab("P(A beats B)") +
  ylim(c(0, 1))

