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


## ----message = FALSE----------------------------------------------------------
library(tidyverse)
url <- paste0("https://web.stanford.edu/~hastie/",
              "ElemStatLearn/datasets/prostate.data")
dataset <- read.table(url)


## -----------------------------------------------------------------------------
# Separate train and test
data_train <- filter(dataset, train)
data_test <- filter(dataset, !train)


## ----message = FALSE----------------------------------------------------------
# Use function lm
fit1 <- lm(lpsa ~ 1, data = data_train)
fit1


## -----------------------------------------------------------------------------
# Getting predicted values
pred_vals1 <- predict(fit1, newdata = data_test)
head(pred_vals1)


## ----message = FALSE----------------------------------------------------------
# Use function lm
fit2 <- lm(lpsa ~ age, data = data_train)
fit2


## -----------------------------------------------------------------------------
# Recall from previous lecture
target <- data_test$lpsa
rmse1 <- sqrt(mean((target - pred_vals1)^2))
rmse1


## -----------------------------------------------------------------------------
pred_vals2 <- predict(fit2, newdata = data_test)
rmse2 <- sqrt(mean((target - pred_vals2)^2))
c(rmse1, rmse2)


## -----------------------------------------------------------------------------
# Remove column train
data_train <- select(data_train, -train)
# The dot is a short-hand for all variables
fit3 <- lm(lpsa ~ ., data = data_train)
fit3


## -----------------------------------------------------------------------------
pred_vals3 <- predict(fit3, newdata = data_test)
rmse3 <- sqrt(mean((target - pred_vals3)^2))
c(rmse1, rmse2, rmse3)


## -----------------------------------------------------------------------------
# Turn gleason into factor
data_train <- mutate(data_train,
                     gleason = factor(gleason))

fit4 <- lm(lpsa ~ ., data = data_train)
fit4


## -----------------------------------------------------------------------------
# gleason needs to be a factor in the test data too!
data_test <- mutate(data_test,
                    gleason = factor(gleason))
pred_vals4 <- predict(fit4, newdata = data_test)
rmse4 <- sqrt(mean((target - pred_vals4)^2))
c(rmse1, rmse2, rmse3, rmse4)


## -----------------------------------------------------------------------------
fit5 <- lm(lpsa ~ lcavol + lweight + age + lbph + 
             svi + lcp + pgg45, data = data_train)

pred_vals5 <- predict(fit5, newdata = data_test)
rmse5 <- sqrt(mean((target - pred_vals5)^2))

c(rmse1, rmse2, rmse3, rmse4, rmse5)


## -----------------------------------------------------------------------------
# poly gives us the polynomial expansion
head(poly(data_train$age, 3))


## -----------------------------------------------------------------------------
fit_cube <- lm(lpsa ~ poly(age, 3), data = data_train)

data_train |> 
  bind_cols(.fitted = fitted(fit_cube)) |> 
  ggplot(aes(x = age)) +
  geom_point(aes(y = lpsa)) +
  geom_line(aes(y = .fitted),
            colour = "blue", size = 1)


## -----------------------------------------------------------------------------
library(splines)
head(bs(data_train$age, 
        knots = c(62, 65, 69),
        degree = 3))


## -----------------------------------------------------------------------------
fit_spl <- lm(lpsa ~ bs(age, degree = 3,
                        knots = c(62, 65, 69)), 
              data = data_train)


## -----------------------------------------------------------------------------
data_train |> 
  bind_cols(.fitted = fitted(fit_spl)) |> 
  ggplot(aes(x = age)) +
  geom_point(aes(y = lpsa)) +
  geom_line(aes(y = .fitted),
            colour = "blue", size = 1) +
  geom_vline(xintercept = c(62, 65, 69),
             linetype = "dashed")


## -----------------------------------------------------------------------------
# The intercept is the 7th df
spl_mat <- bs(data_train$age, df = 6)
head(spl_mat)
attr(spl_mat, "knots")


## -----------------------------------------------------------------------------
# Same result
fit_spl <- lm(lpsa ~ bs(age, df = 6), 
              data = data_train)


## -----------------------------------------------------------------------------
data_train |> 
  bind_cols(.fitted = fitted(fit_spl)) |> 
  ggplot(aes(x = age)) +
  geom_point(aes(y = lpsa)) +
  geom_line(aes(y = .fitted),
            colour = "blue", size = 1) +
  geom_vline(xintercept = c(62, 65, 69),
             linetype = "dashed")


## -----------------------------------------------------------------------------
pred_vals_spl <- predict(fit_spl, newdata = data_test)
rmse_spl <- sqrt(mean((target - pred_vals_spl)^2))

c(rmse2, rmse_spl)

