library(tidyverse)
library(nycflights13)
flights
flights_february <- filter(flights, month==2, day==1) # filter out the flights
flights_february
flightsJanFeb <- filter(flights, month==1 | month==2) #Or operator
flightsJanFeb
#Flights that were NOT delayed by less than two hours 
#(either at arrival or departure)
A.and.B <- filter(flights, arr_delay <= 120, dep_delay <= 120)
A.and.B
alsoA.and.B <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
alsoA.and.B #Use DeMorgan's law
# Select all columns between year and day (inclusive)
select(flights, year:day)

# Excludes some columns
select(flights, -(year:day))

