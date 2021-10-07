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
library(dslabs)

# Rename variables
dataset <- divorce_margarine |> 
  rename(div = divorce_rate_maine,
         marg = margarine_consumption_per_capita)


## -----------------------------------------------------------------------------
# 1. Pearson correlation
dataset |> 
  summarise(sxy = cov(div, marg),
            s2x = var(div),
            s2y = var(marg)) |> 
  mutate(corr = sxy/sqrt(s2x * s2y))


## -----------------------------------------------------------------------------
# Or even simpler
dataset |> 
  summarise(corr = cor(div, marg))


## -----------------------------------------------------------------------------
# 2. Spearman correlation
dataset |> 
  summarise(corr = cor(div, marg, method = "spearman"))


## -----------------------------------------------------------------------------
nobs <- nrow(dataset)
dataset |> 
  mutate(div_rk = rank(div),
         marg_rk = rank(marg),
         diff = div_rk - marg_rk) |> 
  summarise(corr = 1 - 6*sum(diff^2)/(nobs*(nobs^2 - 1)))


## -----------------------------------------------------------------------------
# Also equal to Pearson corr. of ranks
dataset |> 
  mutate(div_rk = rank(div),
         marg_rk = rank(marg)) |> 
  summarise(corr = cor(div_rk, marg_rk))


## ----echo = FALSE-------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(cowplot)

data_vax <- read_csv("vaccines.csv")
data_vax |> 
  filter(Date >= max(Date) - weeks(4)) |> 
  pivot_longer(!Date, names_to = "type",
               values_to = "count") |>
  mutate(type = factor(type, 
                       levels = c("first_dose", "full_dose"),
                       labels = c("First dose", "Fully vacc."))) |> 
  ggplot(aes(x = Date, y = count,
             colour = type)) +
  geom_line() +
  theme_minimal_hgrid() +
  scale_colour_discrete(name = "") +
  theme(legend.position = "top") +
  labs(caption = "Vaccination numbers for SK") +
  xlab("") + ylab("Number of daily doses") +
  scale_y_continuous(labels = scales::comma)


## ----echo = FALSE-------------------------------------------------------------
library(forecast)
data_vax <- data_vax |>
  filter(Date >= max(Date) - weeks(4))

par(mfrow = c(1, 2))
Acf(data_vax$first_dose, main = "First dose", lag.max = 10)
Acf(data_vax$full_dose, main = "Fully vacc.", lag.max = 10)

