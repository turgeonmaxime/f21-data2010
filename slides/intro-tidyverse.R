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


## ----eval = FALSE-------------------------------------------------------------
## # Switch to litres per 100km
## mutate(mtcars, litres_per_100km = 235.215/mpg)


## ----eval = FALSE-------------------------------------------------------------
## # Only keep rows where cyl is equal to 6 or 8
## filter(mtcars, cyl %in% c(6, 8))


## -----------------------------------------------------------------------------
library(tidyverse)

data1 <- mutate(mtcars, litres_per_100km = 235.215/mpg)
data2 <- summarise(data1, 
                   avg_lit = mean(litres_per_100km),
                   sd_lit = sd(litres_per_100km))
data2


## -----------------------------------------------------------------------------
n <- nrow(mtcars)
data3 <- mutate(data2,
                low_bd = avg_lit - 1.96*sd_lit/sqrt(n),
                up_bd = avg_lit + 1.96*sd_lit/sqrt(n))
data3


## -----------------------------------------------------------------------------
library(dslabs)

data1 <- filter(gapminder, 
                year == 2016)
data2 <- summarise(data1, 
                   avg_le = mean(life_expectancy),
                   sd_le = sd(life_expectancy))


## -----------------------------------------------------------------------------
n <- nrow(data1)
data3 <- mutate(data2,
                low_bd = avg_le - 1.96*sd_le/sqrt(n),
                up_bd = avg_le + 1.96*sd_le/sqrt(n))
data3


## ---- message = FALSE, eval = TRUE--------------------------------------------
library(tidyverse)

count(mtcars, cyl)


## ---- message = FALSE, eval = FALSE-------------------------------------------
## # Or with the pipe
## # mtcars becomes the first argument of count
## mtcars %>% count(cyl)


## ----eval = FALSE-------------------------------------------------------------
## # Without pipe operator
## fit_model(prepare_data(dataset))
## # With pipe operator
## dataset %>%
##   prepare_data %>%
##   fit_model


## -----------------------------------------------------------------------------
# Let's convert our previous example to use the pipe
mtcars %>% 
  mutate(litres_per_100km = 235.215/mpg) %>% 
  summarise(avg_lit = mean(litres_per_100km),
            sd_lit = sd(litres_per_100km)) %>% 
  mutate(low_bd = avg_lit - 1.96*sd_lit/sqrt(n),
         up_bd = avg_lit + 1.96*sd_lit/sqrt(n))


## ----eval = TRUE, message = FALSE---------------------------------------------
# Average mpg for each value of cyl
mtcars %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg))


## ----eval = TRUE, message = FALSE---------------------------------------------
# Average mpg for each value of cyl + 95% CI
mtcars %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg),
            sd_mpg = sd(mpg),
            n = n()) %>% 
  mutate(low_bd = avg_mpg - 1.96*sd_mpg/sqrt(n),
         up_bd = avg_mpg + 1.96*sd_mpg/sqrt(n))


## ----eval = FALSE, message = FALSE--------------------------------------------
## # Average mpg for each value of cyl + 95% CI
## mtcars %>%
##   group_by(cyl) %>%
##   summarise(avg_mpg = mean(mpg),
##             sd_mpg = sd(mpg),
##             nobs = n()) %>%
##   mutate(low_bd = avg_mpg - 1.96*sd_mpg/sqrt(nobs),
##          up_bd = avg_mpg + 1.96*sd_mpg/sqrt(nobs))


## -----------------------------------------------------------------------------
gapminder %>% 
  filter(year == 2016) %>% 
  group_by(continent) %>% 
  summarise(avg_le = mean(life_expectancy),
            sd_le = sd(life_expectancy),
            nobs = n()) %>% 
  mutate(low_bd = avg_le - 1.96*sd_le/sqrt(nobs),
         up_bd = avg_le + 1.96*sd_le/sqrt(nobs))

