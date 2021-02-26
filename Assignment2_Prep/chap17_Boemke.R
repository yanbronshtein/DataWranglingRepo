df <- data.frame(var1 = c(10, 25, 8),
                 var2 = c("beer", "wine", "cheese"),
                 var3 = c(TRUE, TRUE, FALSE),
                 row.names = c("billy", "bob", "thornton"))
df

# write to a csv file
write.csv(df, file = "export.csv")

# write to a csv and save in a different directory
write.csv(df, file = "C:\\DataWranglingRepo\\special_folder\\export_csv")

# Write to a csv file with added arguments
write.csv(df, file = "export_txt")

# provides same results as read.delim
write.table(df, file = "export_txt", sep = "\t")



# readr Package
library(readr)

# write to a csv file
write_csv(df, path = "export_csv2")
