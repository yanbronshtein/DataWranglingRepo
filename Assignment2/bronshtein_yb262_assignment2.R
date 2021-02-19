# Import library
library(tidyverse)
# read data from csv remove leading and trailing White space
res_inspec_results <- read_csv(
  "DOHMH_New_York_City_Restaurant_Inspection_Results.csv",
  trim_ws = TRUE)
# Convert data to data frame
df <- as.data.frame(res_inspec_results)
#1a). Form a new data frame restricted to restaurants in Queens with cuisine 
# equal to Pizza
queens_pizza <- filter(df, BORO == "Queens", 
                       str_detect(`CUISINE DESCRIPTION`,
                                  fixed("Pizza", ignore_case = TRUE))) 
dim(queens_pizza)
#1b). What are the 5 most common names (use the variable "DBA") in the data frame?
#(queens_pizza)
common_names <- queens_pizza %>% group_by(DBA) %>% summarise(n=n()) %>% 
  ungroup() %>% arrange(-n)%>%head(5)
common_names
ggplot(data = common_names,
       mapping = aes(x = DBA, y = n)) +
  geom_bar(stat = 'identity', color = "green", fill="purple") +
  labs(
    title = "5 most commonly inspected restaurant names",
    x = "Restaurant Name",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
#1c). On what dates has queens pizza parlor "SUSANO'S PIZZERIA & RESTAURANT"
# been inspected? 
# when I used str_detect, I got 46 entries
queens_pizza_susanos <- filter(queens_pizza,DBA=="SUSANO'S PIZZERIA & RESTAURANT")
View(queens_pizza_susanos)
dates_inspected <- distinct(queens_pizza_susanos["INSPECTION DATE"])
dates_inspected
#(2) The file "gapminder_2007_gini.tsv" is in the “Assignments” folder under 
#the files menu of the course website; it is a subset of the 2007 Gapminder 
#data merged with recent Gini coefficient data 
gapminder_data <- read_tsv("gapminder_2007_gini.tsv", trim_ws = TRUE)
gapminder_data

#2a). Create a plot to compare the distributions 
#(e.g., central tendency, dispersion) of the Gini coefficient in different 
# continents.
gap_df <- as.data.frame(gapminder_data)
ggplot(data = gap_df, mapping = aes(x = continent, y = gini)) +
  geom_boxplot() +
  labs(
    title = "Gini Distribution by Continent"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#(2b) Does the Gini coefficient appear to have any impact on the life 
# expectancy in 2007? Explain your answer using a plot, 
# classified by continents.

special_info <- gap_df %>% filter(year == 2007)  %>% group_by(continent)  %>%
  summarise(continent, gini, lifeExp)

ggplot(data = special_info, mapping = aes(x = gini, y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm", mapping = aes(group = 1)) +
  
  labs(
    title = "Effect of gini on life expectancy by Continent"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# The graph above demonstrates a trend that countries with low gini such as
# Europe and Oceania have much higher life expectancy than Africa which falls
# below the linear regression line





#(3) Using the original gapminder data frame, please generate a data frame 
#with a new variable called gdp by multiplying the population size by the gdp 
#per capita. To make those large numbers more understandable, please form an 
#new variable called gdp_ratio equal to the gdp divided by the gdp of the 
#United States in 2007.
#Find the mean gdp_ratio by continent and year, and then plot the mean 
#gdp_ratio over time, distinguishing the continents. 
# Please use both points and lines for the plot

library(gapminder)
df2 <- gapminder

df2 <- dplyr::mutate(df2, gdp = pop * gdpPercap)
gdp = df2[gdp]
dim(df2)
View(df2)
us_2007_df <- gap_df %>% filter(year==2007, country=="United States")
us_gdp_2007 <- us_2007_df["gdpPercap"] * us_2007_df["pop"]  
denom <- as.numeric(us_gdp_2007)
df2 <- dplyr::mutate(df2, gdp_ratio = gdp / denom )

q3_info <- df2 %>% group_by(continent, year)  %>%
  summarise(continent, year, mean_gdp_ratios = mean(gdp_ratio))

ggplot(data = q3_info, mapping = aes(x = year, y = mean_gdp_ratios, color = continent)) +
  geom_point() +
  geom_line()
View(df2)



