df <- data.frame(col1 = 1:3,
                 col2 = c("this", "is", "text"),
                 col3 = c(TRUE, FALSE, TRUE),
                 col4 = c(2.5, 4.2, pi))

df
str(df)


#number of rows
nrow(df)

#number of cols
ncol(df)

#suppose we don't want automatic factoring for col2: NOTE: THIS DOESN'T ACTUALLY HAPPEN ANYMORE

df <- data.frame(col1 = 1:3,
                 col2 = c("this", "is", "text"),
                 col3 = c(TRUE, FALSE, TRUE),
                 col4 = c(2.5, 4.2, pi),
                 stringsAsFactors = FALSE)

str(df)


#We can also convert pre-existing structures to a dataframe.

v1 <- 1;3
v2 <- c("lets", "have", "fun")
v3 <- c(TRUE, FALSE, TRUE)

#convert same length vecs to data frame using data.frame()
data.frame(col1 = v1, col2 = v2, col3 = v3)


# convert a list to a data frame using as.data.frame()

my_list <- list(item1 = 1:3,
                item2 = c("this", "is", "text"),
                item3 = c(2.5, 4.2, 5.1))
my_list
typeof(my_list)
special_df <- as.data.frame(my_list)
typeof(special_df)

m1 <- matrix(1:12, nrow = 4, ncol = 3)
m1

my_matr <- as.data.frame(m1)
my_matr
typeof(my_matr)



# We can leverage the cbind() function for adding cols

df

# add a new column
v4 <- c("A", "B", "C")
new_df <- cbind(df, v4)
typeof(new_df)


#Use rbind() to add data frame rows together


adding_df <- data.frame(col1 = 4,
                        col2 = "R",
                        col3 = FALSE,
                        col4 = 1.1,
                        stringsAsFactors = FALSE)

df3 <- rbind(df, adding_df)


str(df3)


#Adding attributes to df
attributes(df)
typeof(attributes(df))


rownames(df) <- c("first row", "second row", "third row")

df

attributes(df)

dim(df)
#add/change column names with colnames()
colnames(df) <- c("first col", "second_col", "third col", "fourth col")


attributes(df)


#add comment to a df

comment(df) <- "YOYO you want some comments"
attributes(df)


#Subsetting Data Frames

df
#Subsetting by row numbers

df[2:3, ]


# subsetting by row names
?rownames

rownames(df) <- c("UNO", "DOS", "TRES")
rownames(df)
df[c("DOS", "TRES"),]



# Subsetting columns like a list
df[, c("first col", "fourth col")]


# subset for both rows and columns
df[1:2, c(1,3)]


# Use a vector to subset

v <- c(1, 2, 4)
df[ , v]


#simplifying results in a named vector
df[, 2]

# preserving results in a 3x1 data frame
df[, 2, drop = FALSE]
