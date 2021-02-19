library(tidyverse)

# Now we will work with car mileage data
# stored as mpg
?mpg # Provides information on the data frame

mpg

ggplot() # creates a coordinate system

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# creates a scatter plot of hwy versus displ

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
# Stratifies plot by class (different colors)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# Stratifies plot by class (different shapes)

ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy),shape = 21, colour = "blue", fill = "yellow", size = 2, stroke=1)
# Adding features to the entire plot

# Wrong syntax - will not produce any plot
ggplot(data=mpg)
  + geom_point(mapping = aes(x=displ, y=hwy))
# Wrong syntax - will not produce any plot

