#read.table() multipurpose for importing data
#read.csv() and read.delim() are special cases of read.table()
# in which defaults have been adjusted for efficiency
my_data = read.csv("mydata.csv")
my_data

str(my_data)

# provides same results as read.csv above
read.table("mydata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#set column and row names
read.table("mydata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
           col.names = c("wuf 1", "wuf 2", "wuf 3"),
           row.names = c("ruf 1", "ruf 2", "ruf 3"))

#manually set the classes of the columns
set_classes <- read.table("mydata.csv", sep = ",", header = TRUE,
                          colClasses = c("numeric", "character", "character"))

str(my_data)
str(set_classes)

#limit the number of rows to read in
read.table("mydata.csv", sep = ",", header = TRUE, nrows = 2)


# tab delimited
read.delim("mydata.txt")

read.table("mydata.txt", sep = "\t", header = TRUE)



# 15.1.2 readr Package
library(readr)
mydata_3 = read_csv("mydata.csv")
mydata_3

str(mydata_3)

#specify the column class using col_types
read_csv("mydata.csv", col_types = list(col_double(),
                                        col_character(),
                                        col_character()))

# we can also specify column classes with a string
#in this example d = double, _skips column, c = character
read_csv("mydata.csv", col_types = "d_c")


# set column names
read_csv("mydata.csv", col_names = c("Brody 1", "Brody 2", "Brody 3"), skip = 1)
