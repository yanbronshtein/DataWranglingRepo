library(tidyverse)
mpg
# Cars with big engines use more fuel
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# A graphing template
#1. run ggplot(data = mpg. What do you see?
ggplot(data = mpg)
#I see nothing

#2. How many rows are in mtcars? How many columns
length(mtcars)
dim(mtcars)
# There are 32 rows and 11 columns in mtcars


#3. What does the drv variable describe? Read the help for ?mpg to find out.
?mpg
# The drv variable describes the type of drive train, where f=front-wheel drive,
# r = rear wheel drive, 4 = 4wd
#4. Make a scatterplot of hwy versus cyl
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))
#5. what happens if you make a scatterplot of class versus drv?
# Why is the plot not useful?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))
# This plot is not useful because it does not lend itself to any trend or correlation


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# controls transparency of the points
# top
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Bottom
# ggplot2 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))


#Aesthetic Mappings
#1. What's gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
# The color variable has to be declared outside of the aesthetic
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")


#2. Which variables in mpg are categorical? Which variables are continuous?
?mpg
#manufacturer, model, year, cyl, trans, drv, fl, and class are categorical
#displ, cty, hwy are continuous

#3. Map a continuous variable to color, size, and shape. How do these
#aesthetics behave differently for categorical versus continuous variables?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = "color"))

#4. What happens if you map the same variable to multiple aesthetics?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = displ), color = "blue", shape = 15)
#You get a straight line??
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = displ, z = displ))


?geom_point
#5. What does the stroke aesthetic do? What shapes does it work with
# The stroke aesthetic is used to modify the width of the border
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 17, colour = "black", fill = "white", size = 5, stroke = 5)
#It works with every shape pretty much

#6. What happens if you map an aethetic to something other than a variable name,
# like aes(color = displ < 5)?

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
# It colors the points based on the condition in different colors

#To facet your plot by a single variable, use a facet_wrap()
#First arg is the formula which you create with ~ followed by a variable name
#
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

#To facet the plot on combination of two variables, add facet_grid() to plot coll
#The formula should contain two var names separated by a ~
#If you prefer to not facet in the rows or columns dimesion, use a .
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

#1. What happens if you facet on a continuous variable?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(year ~ manufacturer)
#It generates weird ass graphs?

#2. What do the empty cells in a plot with facet_grid(drv ~ cyl) mean?
# how do they relate to this plot?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl)) +
  facet_grid(drv ~ cyl)
#The empty cells don't do much

#3. What plots does the following code make? What does . do?
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# this code facets on drv
# The next piece of code facets on cyl
?facet_grid


#4. Take the first faceted plot in this sectionl
# What are the advantages
#faceting automatically makes the scale and labels. The disadvantage is 
#that it is harder to customize and the data is actually harder to understand

#5. Read ?facet_wrap. What does nrow do? What does ncol do?
# What other options control the layout of the individual panels?
#Why doesn't facet_grid() have nrow and ncol variables?
?facet_wrap
#nrow/col specify the number of rows and columns in the facet that we make
#dir specifies how the plot will be facing
#levels changes stuff too
# facet_grid calculates rows and columns automatically based on all the variables
#it needs to plot

#6 When using facet_grid() you should usually put the variable with more unique levels
# in the columns. Why?
?facet_grid
#fyi: facet_wrap is used when you have multiple variables in the data.Facet grid
#is when you have two discrete vars and all combos exist in the data

#1. What geom would you use to draw a line chart? A boxplot?
# A histogram? An area chart?
geom_bar()
geom_boxplot()
geom_line()
geom_area()

# Run the code in head a predict output
#
ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point() +
  geom_smooth(se = TRUE) #SE does shading, and geom_smooth() draws a smooth line

ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point()


#3, What does show.legend = FALSE do? What happens if you remove it? Why do you think I used it earlier 
#in the chapter?
#This removes the legend. Might be useful

#4. Se argument sets the shading preferences

#5. Will these two graphs look different? Why/why not?
#the graphs will look the same

#6. Re-create the R code necessary to generate the following graphs


ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)
       ) +
  geom_point() +
  geom_smooth(color="blue", se = FALSE)

ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point() +
  geom_smooth(color = "blue", se = FALSE)

ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv, linetype=drv)
) +
  geom_point() +
  geom_smooth(color = "blue", se = FALSE)

ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point()

ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point()



demo <- tribble(
  ~a,      ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)

ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
  )


ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )

# stat_summary() summarizes the y values for each unique x value,
#to draw attention to the summary that you're computing:
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min, #deprecated
    fun.max = max, #deprecated
    fun.y = median
  )

#1. What is the default geom associated with stat_summary()?
# How could you rewrite the previous plot to use that geom function instead of
# the stat function?
#stat_bin maybe

?stat_summary

#2. What does geom_col() do? How is it different to geom_bar()?
?geom_col
#here are two types of bar charts: geom_bar() and geom_col(). 
#geom_bar() makes the height of the bar proportional to the number of cases 
#in each group (or if the weight aesthetic is supplied, the sum of the weights). 
#If you want the heights of the bars to represent values in the data, use 
#geom_col() instead. geom_bar() uses stat_count() by default: 
#  it counts the number of cases at each x position. 
#geom_col() uses stat_identity(): it leaves the data as is.

#4. stat_smooth() and geom_smooth() do pretty much the same thing
#Use stat_smooth() to show results of non_standard geom
#5. What is the problem with the two graphs
# this plot is fucked up
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

# this plot is fucked up
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = color, y = ..prop.., group = 1)
  )

# This is the correct plot
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )
