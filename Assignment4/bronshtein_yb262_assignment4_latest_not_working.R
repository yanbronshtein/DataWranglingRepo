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
library(measurements)
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

#Read in data by treating it as a tsv from the text file
height_t <- as_tibble(read_tsv("C:\\Users\\Julia\\Downloads\\height.txt"))
#Pull the data into a vector
heights_vec <- height_t %>% pull(height)

# This helper function is used to convert a height given in feet and inches to 
#inches
convert_ft_2_inches <- function(feet, inches) {
  if (feet < 0 || feet >= 12 || inches < 0 || inches >= 12) {
    return(NA)
  }else {
    return(12 * feet + inches)
  }
}

# The function below takes in a vector of heights as strings and outputs
# a numeric vector. The heights are cleaned according to rules specified in
# regular expressions. The failed matches are filled with NA in the returned vector
special_counter <- 0
#clean_height_data <- function(heights_vec) {
  # Create a numeric vector to store cleaned up height data
  cleaned_heights_vec <- numeric(0) 
  for (row in heights_vec) {
    row <- trimws(row)
    
    if (str_detect(row, "^[0-9]*``'$")) {
      cat("Match found feet only", row,"\n")
      
      feet <- as.numeric(str_extract(row, "[0-9]"))
      cleaned_heights_vec <- c(cleaned_heights_vec, convert_ft_2_inches(feet, 0))
      special_counter <- special_counter + 1
    }
    
    # In inches whole number (1)
    else if (str_detect(row, "^[1-9][0-9]{1,2}$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, as.numeric(row))
    }
    # In inches floating point number (2)
    else if (str_detect(row, "^[1-9][0-9]{1,2}\\.[0-9]{1,9}$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, as.numeric(row))
      
    }
    
    # In feet whole number (3)
    else if (str_detect(row, "^[1-9]$")) {
      feet <- as.numeric(str_extract(row, "^[1-9]$"))
      cleaned_heights_vec <- c(cleaned_heights_vec, convert_ft_2_inches(feet, 0))
    }
    # In feet floating point number(4)
    else if (str_detect(row, "^[1-9]{1}[\\.][0-9]{1,5}$")) {
      feet <- as.numeric(str_extract(row, "^[1-9].[1-9]{1,9}$"))
      cleaned_heights_vec <- c(cleaned_heights_vec, convert_ft_2_inches(feet, 0))
    }
    #In inches with ticks
    else if (str_detect(row, "^[0-9]{2,3}(\"|'')$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, 
                               as.numeric(str_extract(row, "[0-9.]+")))
    }
    #In feet with ticks
    else if (str_detect(row, "^[0-9]{1,2}'")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, 
                               as.numeric(str_extract(row, "[0-9.]+")))
    }
      
    #In feet and inches with ticks(5)
    else if (str_detect(row, "^[1-9]{1}'(\\s*[0-9]{1,2}(\\.[0-9]{1,9})?(\"|'')?)?$")) {
      feet_inches <- as.numeric(as.vector(str_extract_all(row,"[0-9.]+", simplify = TRUE)))
      cleaned_heights_vec <- c(cleaned_heights_vec,
                               convert_ft_2_inches(feet_inches[1], feet_inches[2]))
    }
    # In feet and inches with words(6)
    else if (str_detect(row, "^[1-9]{1}\\s*(ft | feet | foot)\\s*[0-9]{1,2}\\s*(in | inches)")) {
      feet_inches <- as.numeric(as.vector(str_extract_all(row,"[0-9.]+", simplify = TRUE)))
      cleaned_heights_vec <- c(cleaned_heights_vec,
                               convert_ft_2_inches(feet_inches[1], feet_inches[2]))

    }
    # In feet and inches with words and floating (7)
    else if (str_detect(row, "^[1-9]{1}\\s*(ft|feet|foot)\\s*[0-9]{1,2}\\.[0-9]{1,9}\\s*(in | inches)$")) {
      feet_inches <- as.numeric(as.vector(str_extract_all(row,"[0-9.]+", simplify = TRUE)))
      cleaned_heights_vec <- c(cleaned_heights_vec,
                               convert_ft_2_inches(feet_inches[1], feet_inches[2]))
    }
    # Height explicitly in centimeters
    else if (str_detect(row, "^[1-9][0-9]{2,3}\\s*(cm|centimeter|centimeters)$")) {
      cm_val <- as.numeric(str_extract(row, "[0-9.]+")) / 2.54
      cleaned_heights_vec <- c(cleaned_heights_vec, cm_val)
    }
    #No possible way to extract match. fill with NA  
    else {
      cleaned_heights_vec <- c(cleaned_heights_vec, NA)
    }
  }
#  return(cleaned_heights_vec)
#}

#Call clean_height_data to extract a cleaned numeric column
out <- clean_height_data(heights_vec)
#Insert the cleaned column back into the height_t as a new column
height_t <- height_t %>% mutate(heights_cleaned=round(out))
height_distributions_t <- height_t %>% select(sex, heights_cleaned) %>%
  group_by(sex, heights_cleaned) %>% arrange(sex, heights_cleaned) %>% summarise(n = n())
#Add counts column

#Derive the count of non-NA entries
count_na <- height_t %>% select(heights_cleaned) %>% is.na() %>% sum %>% as.numeric()
count_not_na <- dim(height_t)[1] - count_na

ggplot(data = height_t, mapping = aes(x = heights_cleaned, y = count(heights_cleaned))) +
  geom_point(aes(color = sex)) +
  labs(
    title = "Distribution of height for Males and females"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

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
  



