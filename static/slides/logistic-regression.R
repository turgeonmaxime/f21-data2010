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


## ----echo = FALSE-------------------------------------------------------------
library(tidyverse)

tibble(p = ppoints(1000)) %>% 
  mutate(logit = log(p/(1-p))) %>% 
  ggplot(aes(x = p, y = logit)) +
  geom_line() +
  theme_minimal() +
  xlab("Probability") +
  ylab("Log odds")


## ----echo = FALSE-------------------------------------------------------------
library(tidyverse)
library(purrr)

expit <- function(t) exp(t)/(1 + exp(t))
xvar <- seq(65, 85)

c(-0.5, 0, 0.5, 1) %>% 
    map_df(function(beta) {
        tibble(height = xvar) %>% 
            mutate(beta = beta,
                   prob = expit(beta*(xvar - 75)))
    }) %>% 
    ggplot(aes(x = height, y = prob)) +
    geom_line() +
    facet_wrap(~beta, labeller = label_both) +
    xlab("Height (in)") + ylab("P(Y = 1 | X)") +
    theme_minimal()


## -----------------------------------------------------------------------------
library(tidyverse)
# Create dataset
dataset <- bind_rows(
  data.frame(Y = rep("right", 43),
             X = rep("male", 43)),
  data.frame(Y = rep("right", 44),
             X = rep("female", 44)),
  data.frame(Y = rep("left", 9),
             X = rep("male", 9)),
  data.frame(Y = rep("left", 4),
             X = rep("female", 4)))


## -----------------------------------------------------------------------------
glimpse(dataset)


## -----------------------------------------------------------------------------
# Outcome must be 0 or 1
dataset <- mutate(dataset, Y = as.numeric(Y=="right"))

glm(Y ~ X, data = dataset,
    family = "binomial")


## -----------------------------------------------------------------------------
# Relationship with odds?
log(11)
log(4.78/11)


## -----------------------------------------------------------------------------
library(Sleuth3)
library(tidyverse)

# First transform outcome to 0/1
dataset <- mutate(case2001,
                  Y = as.numeric(Status == "Survived"))

fit <- glm(Y ~ Age, data = dataset,
           family = "binomial")


## -----------------------------------------------------------------------------
coef(fit)


## -----------------------------------------------------------------------------
library(splines)
fit2 <- glm(Y ~ bs(Age), data = dataset,
            family = "binomial")


## -----------------------------------------------------------------------------
dataset |> 
    mutate(.fitted = fitted(fit),
           .fitted2 = fitted(fit2)) |> 
    ggplot(aes(x = Age)) + 
    geom_point(aes(y = Y)) + 
    geom_line(aes(y = .fitted),
              col = "blue", size = 1) +
    geom_line(aes(y = .fitted2),
              col = "red", size = 1)


## -----------------------------------------------------------------------------
# First create dataset with predictions
data_pred <- bind_rows(
    tibble(truth = factor(dataset$Y),
           estimate = fitted(fit),
           model = "linear"),
    tibble(truth = factor(dataset$Y),
           estimate = fitted(fit2),
           model = "splines")
)


## -----------------------------------------------------------------------------
library(yardstick)

data_pred |> 
    group_by(model) |> 
    roc_curve(truth, estimate,
              event_level = "second") |> 
    autoplot()


## -----------------------------------------------------------------------------
data_pred |> 
    group_by(model) |> 
    roc_auc(truth, estimate,
            event_level = "second")


## -----------------------------------------------------------------------------
data_pred |> 
    group_by(model) |> 
    pr_curve(truth, estimate,
              event_level = "second") |> 
    autoplot() + 
    geom_hline(yintercept = 20/45, 
               linetype = "dashed")


## -----------------------------------------------------------------------------
# PR curve can also be summarized by the area
# under the curve
data_pred |> 
    group_by(model) |> 
    pr_auc(truth, estimate,
           event_level = "second")

