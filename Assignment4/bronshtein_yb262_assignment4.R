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
library(countrycode)
sort_res_by_abs <- function(obj, n = 5) {
  if (!("lm" %in% class(obj))) stop ("obj must have class 'lm'")
  num_res <- length(obj$residuals)
  res <- as_tibble(obj$residuals)
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
heights <- height_t %>% pull(height)
head(heights)
int_match_inches_count <- 0
float_match_inches_count <- 0
int_match_feet_count <- 0
float_match_feet_count <- 0
feet_ticks_match_count <- 0
feet_words_count <- 0
feet_words_count_decimal <- 0

not_match_count <- 0
centimeters_count <- 0

#feet_regex_int <- 
#feet_regex_float

convert_ft_2_inches <- function(feet, inches) {
  if (feet < 0 | feet >= 12 | inches < 0 | inches >= 12) {
    return(NA)
  }
  else {
    return(12 * feet + inches)
  }
}


for (row in heights) {
  row <- trimws(row)
  # In inches whole number (1)
  if (str_detect(row, "^[1-9][0-9]{1,2}$")) {
    
    #cat("Match found integer", row, "\n")
    int_match_inches_count <- int_match_inches_count + 1
  # In inches floating point number (2)
  }else if (str_detect(row, "^[1-9][0-9]{1,2}\\.[0-9]{1,9}$")) {
    #cat("Match found float", row,"\n")
    float_match_inches_count <- float_match_inches_count + 1
    
  }
  # In feet whole number (3)
  else if (str_detect(row, "^[1-9]$")) {
    #cat("Match found feet int", row,"\n")
    feet <- as.numeric(str_extract(row, "^[1-9]$"))
    convert_ft_2_inches(feet, 0)
    int_match_feet_count <- int_match_feet_count + 1
    
  }
  #else if (str_detect(row, "^[1-9]\\.[0-9]{1,5}$")) {
  # In feet floating point number(4)
  else if (str_detect(row, "^[1-9]{1}[\\.][0-9]{1,5}$")) {
    #cat("Match found feet float", row,"\n")
    float_match_feet_count <- float_match_feet_count + 1
    
  }
  #In feet and inches with ticks(5)
  #else if (str_detect(row, "^[1-9]{1}'\\s*[0-9]{1,2}(\" $")) {
  else if (str_detect(row, "^([1-9]{1}'\\s*)?[0-9]{1,2}(\"|'')$")) {
    #cat("Match found between 5'0\" and 5'11\"", row,"\n")
    feet_ticks_match_count <- feet_ticks_match_count + 1
  }
  #}
  # In feet and inches with words(6)
  else if (str_detect(row, "^[1-9]{1}\\s*(ft|feet|foot)\\s*[0-9]{1,2}\\s*(in | inches)$")) {
  #}else if (str_detect(row, "^[1-9]{1}\\s*(ft|feet|feet and|foot)\\s*[0-9]{1,2}\\s*\\.?[0-9]?\\s*(in|inches)")) {
  
  #cat("Match found feet and inches", row,"\n")
    
   feet_words_count <- feet_words_count + 1
    
  
  }
  # In feet and inches with words and floating (7)
  else if (str_detect(row, "^[1-9]{1}\\s*(ft|feet|foot)\\s*[0-9]{1,2}\\.[0-9]{1,9}\\s*(in | inches)$")) {
    cat("Match found feet and inches with decimal", row,"\n")
    feet_words_count_decimal <- feet_words_count_decimal + 1
  }
  # Height explicitly in centimeters
  else if (str_detect(row, "^[1-9][0-9]{2,3}\\s*(cm|centimeter|centimeters)$")) {
    #cat("Match found centimeters", row, "\n")
    centimeters_count <- centimeters_count + 1
    
  }
    
  else {
    cat("Match not found", row, "\n")
    not_match_count <- not_match_count + 1
  }
  
}
cat("match count", match_count)
cat("not match count", not_match_count)



convert_feet_2_inches <- function(height_ft) {
  if (in_feet) {
    return(as.numeric(str_extract(height_ft, pattern)) * 12)
  }
}
extract_height("5",)

## Split the gapminder data by country and use map() to calculate, by country,
#the R-squared for the linear model lifeExp ~ log10(gdpPercap). Using ggplot2,
#make a set of boxplots of R-squared by continent.


gapminder_t <- as_tibble(gapminder)
models_rsquared <- gapminder_t %>% split(.$country) %>%
  map(~ lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .)) %>%
  map(summary) %>% 
  map_dbl(~.$r.squared) %>% 
  as_tibble

continents <- countrycode(sourcevar = gapminder_t %>% 
                                          pull(country) %>% unique(), 
                                         origin = "country.name", 
                                         destination = "continent")


  mutate(countries = gapminder_t %>% select(country) %>% unique()) %>%
  mutate(r_squared = models_rsquared)
  




