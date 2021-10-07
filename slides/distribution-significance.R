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
dataset <- chickwts |> 
  filter(feed %in% c("soybean", "linseed"))

dataset |> 
  group_by(feed) |> 
  summarise(samp_mean = mean(weight),
            std_err = sd(weight)/sqrt(n())) |> 
  mutate(low_bd = samp_mean - 1.96*std_err,
         upp_bd = samp_mean + 1.96*std_err)


## ---- warning = FALSE---------------------------------------------------------
library(infer)
# By default, it assumes unequal variance
dataset |> 
  t_test(formula = weight ~ feed)


## ----warning = FALSE----------------------------------------------------------
library(infer)

gss |> 
  t_test(formula = hours ~ college)


## ----echo = FALSE-------------------------------------------------------------
janitor::tabyl(mtcars, cyl, gear) |> 
  janitor::adorn_title("combined") |> 
  knitr::kable()


## ----echo = FALSE-------------------------------------------------------------
janitor::tabyl(mtcars, cyl, gear) |> 
  janitor::adorn_totals(c("row", "col")) |> 
  janitor::adorn_title("combined") |> 
  knitr::kable()


## ---- warning = FALSE---------------------------------------------------------
# Need to transform gear and cyl into factors
mtcars |> 
  mutate(gear = factor(gear),
         cyl = factor(cyl)) |> 
  chisq_test(formula = gear ~ cyl)


## -----------------------------------------------------------------------------
gss |> 
  chisq_test(college ~ finrela)


## ----echo = FALSE-------------------------------------------------------------
options(knitr.kable.NA = "-")

feed_vct <- levels(chickwts$feed)
p <- length(feed_vct)
mat_pvals <- matrix(NA, nrow = p,
                    ncol = p)

colnames(mat_pvals) <- rownames(mat_pvals) <- feed_vct

for (i in seq(1, p - 1)) {
    xvec <- chickwts$weight[chickwts$feed == feed_vct[i]]
    for (j in seq(i + 1, p)) {
        yvec <- chickwts$weight[chickwts$feed == feed_vct[j]]
        
        mat_pvals[i, j] <- t.test(xvec, yvec)$p.value
    }
}

knitr::kable(mat_pvals[,-1], align = "lccccc",
             digits = c(7, 4, 4, 4, 8))


## ----warning = FALSE----------------------------------------------------------
# calculate the observed statistic
observed_statistic <- gss %>%
  specify(hours ~ college) %>%
  calculate(stat = "t")

observed_statistic


## ----warning = FALSE----------------------------------------------------------
# generate the null distribution with randomization
null_dist_2_sample <- gss %>%
  specify(hours ~ college) %>%
  hypothesize(null = "independence") %>% # Need null=independence
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t")


## ----warning = FALSE----------------------------------------------------------
# calculate the p value from the randomization-based null 
# distribution and the observed statistic
null_dist_2_sample %>%
  get_p_value(obs_stat = observed_statistic,
              direction = "two-sided")

