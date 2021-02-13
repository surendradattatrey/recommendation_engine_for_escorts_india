
rm(list = ls())

library(data.table)
library(dplyr)

# choose Rajasthan data
myFile <- file.choose()
myData  <- read.csv(myFile,header=TRUE)

######################################################################
############# Data Cleaning ##########################################
######################################################################


## Removing last columns
myData <- myData[,-54]
myData <- myData[,-54]
myData <- myData[,-54]
myData <- myData[,-54]
myData <- myData[,-54]
myData <- myData[,-54]

## Removing first columns
myData <- myData[,-1]
myData <- myData[,-1]
a <- as.data.frame(myData)
b <- as.data.table(myData)

# Confirm dimensionality
dim(a) 
dim(b) 
# Unique rows using all columns
dim(unique(a))  
dis_mydata <- as.data.frame(unique(a))
dim(unique(b))  
dis_mydata_u <- as.data.table(unique(b))

#dis_mydata %>%
#  select(dis_mydata$Dlr.Code, dis_mydata$Tehsil, dis_mydata$DSE )

dis_mydata_subset <- dis_mydata[, c(5,14,18,23,29,35)]
#dis_mydata_subset$Status[dis_mydata_subset$Status==3] <- 1
#dis_mydata_subset$Status[dis_mydata_subset$Status==1 | dis_mydata_subset$Status==2 | dis_mydata_subset$Status==4 
#                  | dis_mydata_subset$Status==5 | dis_mydata_subset$Status==6 | dis_mydata_subset$Status==7 
#                  | dis_mydata_subset$Status==8] <- 0

dis_mydata_subset$Status_der<-0

dis_mydata_subset$Status_der <- ifelse(dis_mydata_subset$Status > 3 |dis_mydata_subset$Status < 3 , 0, 1)

write.csv(dis_mydata_subset,'dis_mydata_subset.csv')

dis_mydata_subset <- dis_mydata_subset[,-6]


######################################################################
############# Logistic Regression ####################################
######################################################################

#logistic regression model

## Model 1 - With all variables
model <- glm (Status_der ~ ., data = dis_mydata_subset, family = binomial)
summary(model)

## Model 2 - without Dealer Code variables
model_wo_dlr <- glm (Status_der ~ .-Dlr.Code, data = dis_mydata_subset, family = binomial(link="logit"))
summary(model_wo_dlr)

## Model 3 with Traction effect of DSE and Tehsil
model_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                       , data = dis_mydata_subset, family = binomial(link="logit"))
summary(model_wo_dlr)

## Model 4 other Trial

model_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                     , data = dis_mydata_subset, family = binomial(link="logit"))
summary(model_wo_dlr)






