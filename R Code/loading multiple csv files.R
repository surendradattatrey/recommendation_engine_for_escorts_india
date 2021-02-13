rm(list = ls())

my.files <- list.files(pattern = ".csv")

my.files

for (i in 1:(length(my.files)))
{
  if (i == 1) {
    my.data <- data.frame(NULL)
  }
  
  cur.file <- read.csv(file = my.files[i],skip = 1, sep = "\t")
  
  # I'm using the the column header of the second column as a new variable. This indicator is unique for each file
  cur.file$Name <- names(cur.file)[2]
  
  # before I can append the imported file to my exisitng dummy data frame, I have to make sure the column names match up.
  names(cur.file)[2] <- "Refl"
  
  # using rbind to append the data
  my.data <- rbind(my.data, cur.file)
}
