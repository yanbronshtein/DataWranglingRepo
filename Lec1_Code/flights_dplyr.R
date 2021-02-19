library(tidyverse)
library(nycflights13)
# Now we will work with flights data
flights
?flights

# Flights on February 1, 2013 stored in flightsFeb1
flightsFeb1 = filter(flights, month==2, day==1)
flightsFeb1


# Flights on February 1, 2013 stored in flightsFeb1 (store and display)
(flightsFeb1 = filter(flights, month==2, day==1))

# Add another condition
# Flights in January and February

(flightsJanFeb = filter(flights, month==1 | month==2))

# Alternate way
(flightsJanFeb = filter(flights, month %in% c(1,2)))

# Multiple conditions
# Flights that were NOT delayed by less than two hours
# (either at arrival or departure)

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

# Arrange

# Arrange by year, month, day
arrange(flights, year, month, day)

# Arrange by descending order of departure delay
arrange(flights, dep_delay)

arrange(flights, desc(dep_delay))