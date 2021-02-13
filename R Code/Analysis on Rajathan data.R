rm(list = ls())

setwd("C:/Users/Yash/Desktop/ISB Documents/Capstone Project/Raj Data")

raj_sales_data  <- read.csv("Raj_Data.csv")

head(raj_sales_data)

unique (raj_sales_data$Dealer.Name)

?grep
