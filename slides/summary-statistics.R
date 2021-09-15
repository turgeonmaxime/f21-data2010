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
str(mtcars)


## ---- message = FALSE---------------------------------------------------------
# Extract column with $ and use mean function
mpg_vec <- mtcars$mpg
mean(mpg_vec)


## ---- message = FALSE---------------------------------------------------------
# Or in one line of code
mean(mtcars$mpg)


## -----------------------------------------------------------------------------
mpg_vec <- mtcars$mpg
c(var(mpg_vec), sd(mpg_vec))
IQR(mpg_vec)


## ---- message = FALSE---------------------------------------------------------
# Extract column first
mpg_vec <- mtcars$mpg
median(mpg_vec)


## ---- message = FALSE---------------------------------------------------------
# Or in one line of code
median(mtcars$mpg)


## -----------------------------------------------------------------------------
# Create a vector with NA
vect <- c(1, 4, NA, 5.5)


## -----------------------------------------------------------------------------
mean(vect)
mean(vect, na.rm = TRUE)


## ----eval = TRUE, message = FALSE---------------------------------------------
# Let's look at cyl
table(mtcars$cyl)

mpg_4c <- mtcars$mpg[mtcars$cyl == 4]
mpg_6c <- mtcars$mpg[mtcars$cyl == 6]
mpg_8c <- mtcars$mpg[mtcars$cyl == 8]


## ----eval = TRUE, message = FALSE---------------------------------------------
c(mean(mpg_4c), mean(mpg_6c), mean(mpg_8c))


## ----eval = TRUE, message = FALSE---------------------------------------------
c(median(mpg_4c), median(mpg_6c), 
  median(mpg_8c))


## -----------------------------------------------------------------------------
# Recall----
c(mean(mtcars$mpg), sd(mtcars$mpg), nrow(mtcars))
# 95% Confidence interval
c(20.09062 - 1.96*6.026948/sqrt(32),
  20.09062 + 1.96*6.026948/sqrt(32))


## -----------------------------------------------------------------------------
# Alternative: save values in variables
# and use variables
mean_mpg <- mean(mtcars$mpg)
sd_mpg <- sd(mtcars$mpg)
n <- nrow(mtcars)

c(mean_mpg - 1.96*sd_mpg/sqrt(n),
  mean_mpg + 1.96*sd_mpg/sqrt(n))


## -----------------------------------------------------------------------------
mean_qsec <- mean(mtcars$qsec)
sd_qsec <- sd(mtcars$qsec)
n <- nrow(mtcars)
mean_qsec
sd_qsec

c(mean_qsec - 1.96*sd_qsec/sqrt(n),
  mean_qsec + 1.96*sd_qsec/sqrt(n))


## -----------------------------------------------------------------------------
cyl6 <- as.numeric(mtcars$cyl == 6)
table(cyl6)
mean(cyl6)


## -----------------------------------------------------------------------------
n <- nrow(mtcars)
phat <- mean(cyl6)
sigma <- sqrt(phat*(1 - phat))

c(phat - 1.96*sigma/sqrt(n),
  phat + 1.96*sigma/sqrt(n))


## ----eval = FALSE-------------------------------------------------------------
## # For CSV files----
## library(readr)
## 
## dataset <- read_csv("path/to/file.csv")


## ----eval = FALSE-------------------------------------------------------------
## # For Excel files----
## library(readxl)
## 
## dataset <- read_excel("path/to/file.xlsx")
## # read_excel can also read older xls files


## -----------------------------------------------------------------------------
library(readr)

url <- paste0("https://biostat.app.vumc.org/",
              "wiki/pub/Main/DataSets/",
              "diabetes.csv")


## ----message = TRUE-----------------------------------------------------------
dataset <- read_csv(url)


## -----------------------------------------------------------------------------
# How many rows and columns?
dim(dataset)

# Average height and weight
c(mean(dataset$height), mean(dataset$weight))


## -----------------------------------------------------------------------------
# Missing values!
c(mean(dataset$height, na.rm = TRUE), 
  mean(dataset$weight, na.rm = TRUE))

