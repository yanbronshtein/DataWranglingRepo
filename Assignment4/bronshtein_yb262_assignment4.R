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


# This helper function is used to convert a height given in feet and inches to 
#inches
convert_ft_2_inches <- function(feet, inches=0) {
  if (feet < 0 || feet >= 12 || inches < 0 || inches >= 12) {
    return(NA)
  }else {
    return(12 * feet + inches)
  }
}

# Read in height data
height_t <- as_tibble(read_tsv("C:\\Users\\Julia\\Downloads\\height.txt")) %>% 
  select(sex, height)
heights_vec <- height_t %>% pull(height)

# Initialize counters
int_match_inches_count <- 0
float_match_inches_count <- 0
int_match_feet_count <- 0
float_match_feet_count <- 0
feet_ticks_match_count <- 0
feet_words_count <- 0
feet_words_count_decimal <- 0
feet_only_ticks <- 0
not_match_count <- 0
centimeters_count <- 0


# Thus function is used to perform cleaning of the height vector using regexes
# The input is a character vector and the output is a numeric vector
clean_height_data <- function(heights_vec) {
  # Create a numeric vector to store cleaned up height data
  cleaned_heights_vec <- numeric(0) 
  for (row in heights_vec) {
    row <- trimws(row)
    #In feet only with ticks
    if (str_detect(row, "^[1-9]'$")) {
      str_vec <- as.vector(str_extract_all(row, "[0-9.]+", simplify = TRUE))
      if (length(str_vec) ==1) {
        feet <- as.numeric(str_vec[1])
        result <- convert_ft_2_inches(feet)
        cleaned_heights_vec <- c(cleaned_heights_vec, result)
      } 
    }
    # In inches whole number (1)
    else if (str_detect(row, "^[1-9][0-9]{1,2}$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, as.numeric(row))
      int_match_inches_count <- int_match_inches_count + 1
    }
    # In inches floating point number (2)
    else if (str_detect(row, "^[1-9][0-9]{1,2}\\.[0-9]{1,9}$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, as.numeric(row))
      float_match_inches_count <- float_match_inches_count + 1
      
    }
    
    # In feet whole number (3)
    else if (str_detect(row, "^[1-9]$")) {
      feet <- as.numeric(str_extract(row, "^[1-9]$"))
      cleaned_heights_vec <- c(cleaned_heights_vec, convert_ft_2_inches(feet, 0))
      int_match_feet_count <- int_match_feet_count + 1
    }
    # In feet floating point number(4)
    else if (str_detect(row, "^[1-9]{1}[\\.][0-9]{1,5}$")) {
      feet <- as.numeric(str_extract(row, "^[1-9].[1-9]{1,9}$"))
      cleaned_heights_vec <- c(cleaned_heights_vec, convert_ft_2_inches(feet, 0))
      float_match_feet_count <- float_match_feet_count + 1
    }
    #In inches only with ticks
    else if (str_detect(row, "^[0-9]{2,3}(\"|'')$")) {
      cleaned_heights_vec <- c(cleaned_heights_vec, 
                               as.numeric(str_extract(row, "[0-9.]+")))
      
    }
    #In feet and inches with ticks(5)
    else if (str_detect(row, "^[1-9]{1}'([0-9]{1,2}(\\.[0-9]{1,9})?(\"|'')?)?$")) {
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
  return(cleaned_heights_vec)
}

#Call clean_height_data to extract a cleaned numeric column
out <- clean_height_data(heights_vec)
#Insert the cleaned column back into the height_t as a new column
height_t <- height_t %>% mutate(heights_cleaned=round(out)) %>%
  select(sex, heights_cleaned) %>% rename(height = heights_cleaned)

#Derive the count of non-NA entries
count_na <- height_t %>% select(height) %>% is.na() %>% sum %>% as.numeric()
count_not_na <- dim(height_t)[1] - count_na

# Create male plot
height_males_t <- height_t %>% filter(sex=="Male") %>% group_by(height) %>% 
  summarise(sex = sex, n = n())

# Create the female plot
height_females_t <- height_t %>% filter(sex=="Female") %>% group_by(height) %>% 
  summarise(sex = sex, n = n())

#Create the combined data from the two data frames
combined_data <- bind_rows(height_males_t, height_females_t) %>% filter(!is.na(height))
ggplot(data = combined_data, mapping = aes(x = height, y = n)) +
  geom_point(mapping = aes(color=sex)) +
  labs (
    title = "Male and female height distribution",
    x = "Height in inches",
    y = "Count"
  )  +
  theme(plot.title = element_text(hjust = 0.5))

#question 3:
## Split the gapminder data by country and use map() to calculate, by country,
#the R-squared for the linear model lifeExp ~ log10(gdpPercap). Using ggplot2,
#make a set of boxplots of R-squared by continent.

# Extract the values of R_squared for each country by performing the following steps:
#a). call split() to split the gapminder tibble data into separate tibbles by country
#b). Call map() to apply lm() which creates a linear model based on the equation
# lifeExp ~ log10(gdpPercap)
#Make sure to set the weights field to population
#Use map yet again to call summary() on every lm() object
# Now we call map_dbl() to create a list of all the R^2
gapminder_t <- as_tibble(gapminder)

countries_rsquared <- gapminder_t %>% 
  split(.$country) %>%
  map(~ lm(lifeExp ~ log10(gdpPercap), weights = pop, data = .)) %>%
  map(summary) %>% 
  map_dbl("r.squared")


#Now we need to extract all the country names of the countries and all the r^2 
#values to create a vertical tibble. We perform a left_join with the original gap_minder
# tibble to get all the original columns
continent_countries_t <- countries_rsquared %>%
  tibble(country = names(.), R_squared = .) %>%
  left_join(gapminder_t)


#For the tibble needed for the ggplot() we only need the columns continent and R_squared
final_t <- continent_countries_t %>% select(continent, R_squared)

# Generate the ggplot()
ggplot(data = final_t, mapping = aes(x = R_squared, y = continent)) +
  geom_boxplot()
  