rm(list = ls())

setwd("C:/Users/Yash/Desktop/ISB Documents/Capstone Project/Raj Data")
my.files <- list.files(pattern = ".csv")

dat <- NULL

for(i in 1:2)
{
  dat<- rbind(dat, read.csv(my.files[i]))
}

head (dat)

write.csv(dat, file = "Raj_Data.csv")

