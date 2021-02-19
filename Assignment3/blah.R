fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", toupper)
str_replace_all(fruits, "b", NA_character_)

str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace_all(fruits, c("a", "e", "n"), "-")

# If you want to apply multiple patterns and replacements to the same
# string, pass a named vector to pattern.
fruits %>%
  str_c(collapse = "---") %>%
  str_replace_all(c("one" = "1", "two" = "2", "three" = "3"))

# Use a function for more sophisticated replacement. This example
# replaces colour names with their hex values.
colours <- str_c("\\b", colors(), "\\b", collapse="|")
col2hex <- function(col) {
  rgb <- col2rgb(col)
  rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
}

x <- c(
  "Roses are red, violets are blue",
  "My favourite colour is green"
)


library(tidyverse)
quadratic1 = function(a, b, c) {
  r1 = (-b + sqrt(b^2 -4*a*c)) / (2*a)
  r2 = (-b - sqrt(b^2 -4*a*c)) / (2*a)
  return(c(r1,r2))
}

quadratic2 = function(a, b, c) {
  discrim <- b^2 -4*a*c
  if (discrim >= 0) {
    r1 <- (-b + sqrt(discrim)) / (2*a)
    r2 <- (-b - sqrt(discrim)) / (2*a)
    
  } else {
    real_part <- -b / (2*a)
    complex_part <- sqrt(-b^2  + 4*a*c) / (2*a)
    r1 = complex(real = real_part, imaginary = complex_part)
    r2 = complex(real = real_part, imaginary = -complex_part)
    
  }
  return(c(r1,r2))
}


reformat_phone_number <- function(x, sep = "-") {
  str_replace_all(x, "\\(?([2-9][0-9]{2})\\)?[- .]?([0-9]{3})[- .]?([0-9]{4})", 
                  paste0("\\2", sep, "\\1", sep, "\\3"))
}

reformat_phone_number2 <- function(x, sep = "-"){
  str_replace_all(x, "\\(?([2-9][0-9]{2})\\)?[- .]?([0-9]{3})[- .]?([0-9]{4})", paste0("\\1",
                                                                                       sep, "\\2", sep, "\\3"))
}



quadratic1(1,-5,6)
quadratic2(1,1,1)


reformat_phone_number2(c("3214567890", "(321) 456-7890"))
reformat_phone_number(c("7188642667", "(718) 864-2667"))
reformat_phone_number("7188642667")


