library(nycflights13)
library(tidyverse)
flights
View(flights)
(first_filter <- filter(flights, month == 1, day == 1))
#dim(first_filter)
NA==NA
NA != NA

x <- NA
y <- NA
df <- tibble(x == c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)


#1. Find all flights tha:
#a. Had an arrival delay of two or more hours
two_hour_delay_flights <- filter(flights, flights$arr_delay >= 2)


#b. Flew to Houston (IAH or HOU)
flew_to_houston_flights <- filter(flights, dest %in% c("IAH","HOU"))
flew_to_houston_flights
View(flew_to_houston_flights)

#c. Were operated by United, American, or Delta
united_american_delta <- filter(flights, flights$carrier %in% c("UA", "AA", "DL"))
View(united_american_delta)
c(flights$carrier)
