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


## -----------------------------------------------------------------------------
# Model without gleason
fit <- lm(lpsa ~ lcavol + lweight + age + lbph + 
            svi + lcp + pgg45, data = data_train)
coef(fit)


## -----------------------------------------------------------------------------
# For ridge regression, we need the model matrix
# and the vector y
X <- model.matrix(fit)
y <- data_train$lpsa


## -----------------------------------------------------------------------------
beta_ols <- solve(crossprod(X)) %*%
  crossprod(X, y)
all.equal(coef(fit), beta_ols[,1])


## -----------------------------------------------------------------------------
lambda <- 1.0
p <- ncol(X)
beta_ridge <- solve(crossprod(X) + diag(lambda,
                                        ncol = p,
                                        nrow = p)) %*%
  crossprod(X, y)

beta_ridge[,1]


## -----------------------------------------------------------------------------
# Use the same formula as for fitting the model
X_test <- model.matrix(~ lcavol + lweight + age + 
                          lbph + svi + lcp + pgg45, 
                       data = data_test)
X_test <- as.matrix(X_test)
dim(X_test)


## -----------------------------------------------------------------------------
y_test <- data_test$lpsa
y_pred <- X_test %*% beta_ridge

rmse <- sqrt(mean((y_test - y_pred)^2))
rmse


## -----------------------------------------------------------------------------
# Compare to OLS estimate
pred_vals <- predict(fit, newdata = data_test)
sqrt(mean((y_test - pred_vals)^2))


## ----echo = FALSE, cache = TRUE-----------------------------------------------
lambda_vect <- seq(0, 5, by = 0.1)

rmse_vect <- sapply(lambda_vect, function (lambda) {
  beta_ridge <- solve(crossprod(X) + lambda*diag(p)) %*%
  crossprod(X, y)
  y_pred <- X_test %*% beta_ridge

  return(sqrt(mean((y_test - y_pred)^2)))
})

tibble(lambda = lambda_vect,
       RMSE = rmse_vect) |> 
  ggplot(aes(x = lambda, y = RMSE)) +
  geom_line()


## ----echo = FALSE, cache = TRUE-----------------------------------------------
library(purrr)

beta_df <- map_dfr(lambda_vect, function (lambda) {
  beta_ridge <- solve(crossprod(X) + diag(lambda,
                                        ncol = p,
                                        nrow = p)) %*%
  crossprod(X, y)

  output <- bind_cols(tibble(lambda = lambda),
                      as_tibble(t(beta_ridge)))
  return(output)
})

beta_df |> 
  pivot_longer(cols = -c("lambda"),
               names_to = "Covariate",
               values_to = "Estimate") |> 
  ggplot(aes(x = lambda, y = Estimate,
             colour = Covariate)) +
  geom_line() +
  theme(legend.position = 'top') +
  geom_hline(yintercept = 0, linetype = "dashed")


## -----------------------------------------------------------------------------
library(glmnet)
# We need to remove the intercept from X
X <- X[, -1]

fit_lasso <- glmnet(X, y)


## -----------------------------------------------------------------------------
# Note: The x-axis is on the log scale
plot(fit_lasso, xvar = "lambda")


## -----------------------------------------------------------------------------
# Use predict to get predictions
X_test <- X_test[, -1]
pred_lasso <- predict(fit_lasso, newx = X_test,
                      s = 1.0) # s = lambda

rmse_lasso <- sqrt(mean((y_test - pred_lasso)^2))
rmse_lasso


## -----------------------------------------------------------------------------
pred_lasso <- predict(fit_lasso, newx = X_test,
                      s = 0.11) # s = lambda

sqrt(mean((y_test - pred_lasso)^2))

