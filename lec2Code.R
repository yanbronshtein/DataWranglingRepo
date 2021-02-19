#Plot the arrival delays(y-axis) against departure delays(x-axis), using
#color labels for the three origin airports


#Add a horizaontal line x=y(45 degrees) to the plot.
library(tidyverse)
library(nycflights13)
#flights_plot <- ggplot(
#  data = filter(flights, year == 2013, month == 9, day == 12),
#  aes(dep_delay, arr_delay)) +
#  geom_point(aes(color = origin))
#)

#flights_plot + geom_line(aes(dep_delay, dep_delay))
library(lubridate)
ymd("12/3/1")

#specify timezone

ymd_hms("2012-03-01 12:23:15")
