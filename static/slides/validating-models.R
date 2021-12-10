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
url <- paste0("https://web.stanford.edu/~hastie/",
              "ElemStatLearn/datasets/prostate.data")
data <- read.table(url)

names(data)


## -----------------------------------------------------------------------------
# Separate train and test
library(tidyverse)
count(data, train)

data_train <- filter(data, train)
data_test <- filter(data, !train)


## -----------------------------------------------------------------------------
prediction <- data_train |> 
  pull(lpsa) |> 
  mean(na.rm = TRUE)

prediction


## -----------------------------------------------------------------------------
# Now compute metrics----
actual_vals <- pull(data_test, lpsa)
# MSE
mse <- mean((actual_vals - prediction)^2)
rmse <- sqrt(mse)

c(mse, rmse)


## -----------------------------------------------------------------------------
# MAE
mae <- mean(abs(actual_vals - prediction))
mae


## -----------------------------------------------------------------------------
# MAPE
ratios <- (actual_vals - prediction)/actual_vals
mape <- 100*mean(abs(ratios))
mape


## -----------------------------------------------------------------------------
# All together
c(mse, rmse, mae, mape)

