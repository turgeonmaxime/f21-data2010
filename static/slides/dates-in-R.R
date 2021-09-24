## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(lubridate)
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
library(lubridate)
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")


## -----------------------------------------------------------------------------
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")


## -----------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)

flights %>% 
  select(year, month, day, hour, minute)


## -----------------------------------------------------------------------------
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, 
                                   hour, minute))


## -----------------------------------------------------------------------------
# Offset in seconds
as_datetime(60 * 60 * 10)
# Offset in days
as_date(365 * 10 + 2)


## ----eval = FALSE-------------------------------------------------------------
## ymd(c("2010-10-10", "bananas"))


## -----------------------------------------------------------------------------
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014


## ----eval = TRUE--------------------------------------------------------------
ymd(c("2010-10-10", "bananas"))


## -----------------------------------------------------------------------------
mdy("January 1, 2010")
ymd("2015-Mar-07")


## -----------------------------------------------------------------------------
dmy("06-Jun-2017")
mdy(c("August 19 (2015)", "July 1 (2015)"))
mdy("12/30/14")


## -----------------------------------------------------------------------------
month(today(), label = TRUE)

wday(now(), label = TRUE, abbr = FALSE)


## -----------------------------------------------------------------------------
# Simple arithmetic
today() + years(1)
today() + weeks(4)


## -----------------------------------------------------------------------------
# But be careful! 
ymd("2021/01/31") + months(1)


## -----------------------------------------------------------------------------
ceiling_date(today(), unit = "month")
floor_date(today(), unit = "year")


## -----------------------------------------------------------------------------
# Careful: durations are an exact number of seconds!
one_pm <- ymd_hms("2016-03-12 13:00:00", 
                  tz = "America/New_York")

one_pm
one_pm + ddays(1)


## -----------------------------------------------------------------------------
next_year <- today() + years(1)
today() %--% next_year


## -----------------------------------------------------------------------------
(today() %--% next_year)/ddays(1)
(today() %--% next_year)/dweeks(1)


## -----------------------------------------------------------------------------
ymd("20150101") + months(0:11)


## -----------------------------------------------------------------------------
floor_date(today(), unit = "years") + months(0:11)


## -----------------------------------------------------------------------------
# Also valid
first_day <- ymd(paste0(year(today()), "0101"))
first_day + months(0:11)


## -----------------------------------------------------------------------------
calculate_age <- function(date_birth) {
  age <- (date_birth %--% today())/dyears(1)
  # Round down
  return(floor(age))
}

calculate_age(ymd("20010911"))


## -----------------------------------------------------------------------------
# Setting the time zone attribute
x1 <- ymd_hms("2021-09-20 16:00:00", 
              tz = "America/Winnipeg")
x2 <- ymd_hms("2021-09-20 23:00:00", 
              tz = "Europe/Copenhagen")
x3 <- ymd_hms("2021-09-21 9:00:00", 
              tz = "Pacific/Auckland")


## -----------------------------------------------------------------------------
# These are all the same instant in time
x1 - x2
x1 - x3


## -----------------------------------------------------------------------------
# Convert
x1
with_tz(x1, tzone = "America/St_Johns")
force_tz(x1, tzone = "America/Edmonton")

