library(babynames)
library(tidyverse)



#babynames

baby_Taylor = filter(babynames, name=="Taylor")
ggplot(data = baby_Taylor) +
  geom_line(mapping=aes(x=year, y=n, color=sex)) +
  labs(title = "Male and Female babies named Taylor by year",
       x = "years",
       y = "# of babies")



#Answer the following questions, showing plots to substantiate your answers: 
#Is a 16 year old named Quinn more likely to be a boy or a girl?
max_year <- max(babynames$year)
year_16 <- max_year - 16

baby_Quinn = filter(babynames, name=="Quinn", year>= year_16)
ggplot(data = baby_Quinn) +
  geom_line(mapping=aes(x=year, y=n, color=sex)) +
  labs(title = "Male vs Female Trend of baby name Quinn",
       x = "Year",
       y = "# of babies") +
  scale_x_continuous(breaks = seq(year_16, max_year, by = 2))
#To answer this question, one looks at 2017 and sees that the red line(females)
#is at an all-time high, meaning that Quinn will likely be born a  girl



#Is a 2 year old named Quinn more likely to be a boy or a girl?
# Based on the lineplot created for the previous question, in 2003, when Quinn
# would be 2 years old, they would most likely be male

#What is your best guess as to how old a woman named Susan is?
#Susan was mostly likely born between 1950 and 1960 making her between 57 and 67

susan_female <- filter(babynames, name=="Susan", sex == "F")
ggplot(data = susan_female) +
  geom_line(mapping=aes(x=year, y=n)) +
  labs(title = "Trend for a female for the baby name Susan",
       x = "Year",
       y = "# of babies")



