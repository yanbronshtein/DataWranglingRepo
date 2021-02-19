#lm(formula, data, subset, weights, na.action,
#   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#   singular.ok = TRUE, contrasts = NULL, offset, ...)
#Arguments

#Create a new function that, given an `lm` object, returns the top n residuals  
#arranged in descending order according to their largest absolute values  
#(but returns the residuals, not the absolute value of the residuals),  
#where the default value for n is 5. The function should give a clear error  
#message if n is larger than the number of residuals   
#Demonstrate that your function works by applying it to
#mtcars.lm <- lm(mpg ~ disp, data = mtcars)
#first with no argument for n, then with n = 6, and then with n = 40 (error message expected).

library(tidyverse)
library(gapminder)
sort_res_by_abs <- function(obj, n = 5) {
  if (!("lm" %in% class(obj))) stop ("obj must have class 'lm'")
  num_res <- length(obj$residuals)
  res <- as_tibble(obj$residuals)
  print("Num res")
  print(num_res)
  if (n > num_res) {
    stop (paste("the n value", n, "is larger than the number of residuals", 
                num_res, sep = " "))
  } 
  sorted_res <- res %>% rename(res = value) %>% 
    mutate(abs_res = abs(res)) %>%
    arrange(desc(abs_res)) %>% 
    select(res) %>% head(n)
  return(as.list(sorted_res))
  
}

mtcars.lm <- lm(mpg ~ disp, data = mtcars)
sort_res_by_abs(mtcars.lm, 20)

## Question 2
#Read the file "height.txt" in the folder, and use regular expressions to 
#clean the height variable and convert it into a numeric column representing 
#height in inches. Determine and report the number of non-missing values of 
#height for men and women. Finally, make a plot showing the distributions of 
#height for men and for women on the same plot.
height_t <- as_tibble(read_tsv("C:\\Users\\Julia\\Downloads\\height.txt"))
height_t


## Split the gapminder data by country and use map() to calculate, by country,
#the R-squared for the linear model lifeExp ~ log10(gdpPercap). Using ggplot2,
#make a set of boxplots of R-squared by continent.


gapminder_t <- as_tibble(gapminder)
models_rsquared <- gapminder_t %>% split(.$country) %>%
  map(~ lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .)) %>%
  map(summary) %>% map_dbl(~.$r.squared)


