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


## ---- echo = FALSE------------------------------------------------------------
cat("Number of cars by number of cylinders and gears")
janitor::tabyl(mtcars, cyl, gear)


## -----------------------------------------------------------------------------
library(tidyverse)

mtcars |> 
  count(cyl, gear)


## -----------------------------------------------------------------------------
library(tidyverse)
# Create test dataset
dataset <- tribble(
  ~cyl, ~"3", ~"4", ~"5",
  4, 1, 8, 2,
  6, 2, 4, 1,
  8, 12, 0, 2
)


## -----------------------------------------------------------------------------
dataset


## -----------------------------------------------------------------------------
# Pivot long-from tabular to tidy
dataset |> 
  pivot_longer(cols = c(`3`, `4`, `5`),
               names_to = "gear",
               values_to = "count")


## -----------------------------------------------------------------------------
library(tidyverse)

mtcars |> 
  group_by(cyl, gear) |> 
  summarise(count = n()) |> 
  pivot_wider(names_from = "gear",
              values_from = "count")


## -----------------------------------------------------------------------------
# Group by cyl, gear
# Use summarise to count
# Then pivot wide

mtcars |> 
  group_by(cyl, gear) |> 
  summarise(count = n()) |> 
  pivot_wider(names_from = "gear",
              values_from = "count", 
              values_fill = 0) # Fill missing with 0


## -----------------------------------------------------------------------------
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)


## -----------------------------------------------------------------------------
preg |> 
  pivot_longer(cols = c("male", "female"),
               names_to = "sex",
               values_to = "count")


## -----------------------------------------------------------------------------
library(tidyverse)
dataset <- tribble(
  ~country, ~year, ~rate,             
  "Afghanistan", 1999,  "745/19987071",     
  "Afghanistan", 2000,  "2666/20595360",    
  "Brazil",      1999,  "37737/172006362",  
  "Brazil",      2000,  "80488/174504898",  
  "China",       1999,  "212258/1272915272",
  "China",       2000,  "213766/1280428583"
)


## -----------------------------------------------------------------------------
dataset |> 
  separate(rate, into = c("cases", "population"))


## -----------------------------------------------------------------------------
dataset |> 
  separate(rate, into = c("cases", "population"),
           sep = "/", convert = TRUE)


## -----------------------------------------------------------------------------
dataset <- tribble(
  ~Country, ~m014, ~m1524, ~f014, ~f1524,
  "USA", 2, 4, 4, 6,
  "France", 52, 228, 183, 149
)


## -----------------------------------------------------------------------------
# Socio-demographic variable -> socio
# Cell values -> value (don't know what they represent)
dataset |> 
  pivot_longer(cols = c("m014", "m1524", 
                        "f014", "f1524"),
               names_to = "socio", 
               values_to = "value") |> 
  separate(socio, into = c("sex", "age"),
           sep = 1)


## -----------------------------------------------------------------------------
dataset <- tribble(
  ~Country, ~Stat, ~Y2020, ~ Y2021,
  "Nigeria", "GDP", 400, 450,
  "Nigeria", "Debt", 1200, 1300,
  "Bangladesh", "GDP", 350, 400,
  "Bangladesh", "Debt", 100, 150
)

dataset


## -----------------------------------------------------------------------------
dataset |> 
  pivot_longer(cols = c("Y2020", "Y2021"),
               names_to = "year", values_to = "value") |> 
  pivot_wider(names_from = "Stat", values_from = "value")


## -----------------------------------------------------------------------------
# First year of data
data_year1 <- tribble(
  ~Country, ~sex, ~value,
  "USA", "male", 2, 
  "USA", "female", 4,
  "France", "male", 52,
  "France", "female", 183
)


## -----------------------------------------------------------------------------
# Second year of data
# Note: column names are different!
data_year2 <- tribble(
    ~Country, ~sex, ~score,
  "USA", "male", 4, 
  "USA", "female", 6,
  "France", "male", 228,
  "France", "female", 149
)


## -----------------------------------------------------------------------------
# First change name, then
# create new variable
data_year1 <- data_year1 |> 
  rename(score = value) |> # old = new
  mutate(year = 1)

data_year2 <- data_year2 |> 
  mutate(year = 2)


## -----------------------------------------------------------------------------
# Then stack them
bind_rows(data_year1, data_year2)

