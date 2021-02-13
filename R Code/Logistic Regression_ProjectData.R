rm(list = ls())

library(data.table)
library(dplyr)
library(pscl)
library(caret)
library(ROCR)
require(MASS)

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
#dim(unique(b))  
dis_mydata_u <- as.data.table(unique(b))

#dis_mydata %>%
#  select(dis_mydata$Dlr.Code, dis_mydata$Tehsil, dis_mydata$DSE )

dis_mydata_subset <- dis_mydata[, c(5,14,18,23,29,35)]

dis_mydata_subset$Status_der<-0

dis_mydata_subset$Status_der <- ifelse(dis_mydata_subset$Status > 3 |dis_mydata_subset$Status < 3 , 0, 1)

#dis_mydata_subset$Status_der <- as.factor(dis_mydata_subset$Status_der)

#write.csv(dis_mydata_subset,'dis_mydata_subset.csv')

dis_mydata_subset <- dis_mydata_subset[,-6]


######################################################################
############# Logistic Regression ####################################
######################################################################
############# Splitting data between Train and Test ##################

dt = sort(sample(nrow(dis_mydata_subset), nrow(dis_mydata_subset)*.7))
train_total<-dis_mydata_subset[dt,]
test_total<-dis_mydata_subset[-dt,]


## Model 1 - With all variables
model <- glm (Status_der ~ .
              , data = train_total, family = binomial)
summary(model)

best_model <- stepAIC (model, trace = FALSE)
best_model$anova

forward_model <- stepAIC(model,direction = "forward", trace = FALSE)
forward_model$anova

backward_model <- stepAIC(model,direction = "backward", trace = FALSE)
backward_model$anova


fitted.results <- predict(model,newdata=test_total[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_total$Status_der)
print(paste('Accuracy',1-misClasificError))

########### Total Accuracy on whole data is 92.43% #########################

fitted.results <- predict(model,newdata=test_total[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError_overall <- mean(fitted.results != test_total$Status_der)
print(paste('Accuracy',1-misClasificError_overall))

####################################################################################################
############       Segeregating data based on dealer Category      #################################
####################################################################################################

agg_agency_data <- aggregate(dis_mydata_subset$Status_der, 
                             by=list(Category=dis_mydata_subset$Dlr.Code), FUN=sum)
agg_agency_data

agency_sale_more_than_250 <- filter(agg_agency_data, x > 250)
agency_sale_more_than_250

agency_sale_more_than_250_dtl <- subset (dis_mydata_subset, 
                                         dis_mydata_subset$Dlr.Code == 
                                           agency_sale_more_than_250$Category)
agency_sale_more_than_250_dtl


##################################################################################################
################## Analysis for Agenciew which sales is more than 250 Tractors ###################
##################################################################################################

dt_greater_250 = sort(sample(nrow(agency_sale_more_than_250_dtl), 
                             nrow(agency_sale_more_than_250_dtl)*.7))
train_greater_250<-agency_sale_more_than_250_dtl[dt_greater_250,]
test_greater_250<-agency_sale_more_than_250_dtl[-dt_greater_250,]

model_250 <- glm (Status_der ~ .
              , data = train_greater_250, family = binomial)
summary(model_250)

best_model_250 <- stepAIC (model_250, trace = FALSE)
best_model_250$anova

forward_model_250 <- stepAIC(model_250,direction = "forward", trace = FALSE)
forward_model_250$anova

backward_model_250 <- stepAIC(model_250,direction = "backward", trace = FALSE)
backward_model_250$anova


fitted.results_250 <- predict(best_model_250,newdata=test_greater_250[,],type='response')
fitted.results_250 <- ifelse(fitted.results_250 > 0.5,1,0)
misClasificError_250 <- mean(fitted.results_250 != test_greater_250$Status_der)
print(paste('Accuracy',1-misClasificError_250))



########### Total Accuracy on whole data is 90.20% #########################

###############################################################################################
####################### Sales beween 100 to 250 ###############################################
###############################################################################################

agency_sale_between_100_to_250 <- filter(agg_agency_data, x < 250 & x > 100)
agency_sale_between_100_to_250

agency_sale_between_100_to_250_dtl <- subset (dis_mydata_subset, dis_mydata_subset$Dlr.Code == agency_sale_between_100_to_250$Category)
agency_sale_between_100_to_250_dtl

############################################################################################
###################### Splitting data between Train and Test ###############################


agency_sale_between_100_to_250_dtl

dt_between_100_250 = sort(sample(nrow(agency_sale_between_100_to_250_dtl)
                                 , nrow(agency_sale_between_100_to_250_dtl)*.7))
train_between_100_250 <- agency_sale_between_100_to_250_dtl[dt_between_100_250,]
test_between_100_250 <- agency_sale_between_100_to_250_dtl[-dt_between_100_250,]


model_between_100_250 <- glm (Status_der ~ .
                  , data = train_between_100_250, family = binomial)
summary(model_between_100_250)

best_model_between_100_250 <- stepAIC (model_between_100_250, trace = FALSE)
best_model_between_100_250$anova

forward_model_between_100_250 <- stepAIC(model_between_100_250,direction = "forward", trace = FALSE)
forward_model_between_100_250$anova

backward_model_between_100_250 <- stepAIC(model_between_100_250,direction = "backward", trace = FALSE)
backward_model_between_100_250$anova


fitted.results_between_100_250 <- predict(best_model_between_100_250,newdata=test_total[,],type='response')
fitted.results_between_100_250 <- ifelse(fitted.results_between_100_250 > 0.5,1,0)
misClasificError_between_100_250 <- mean(fitted.results_between_100_250 != test_between_100_250$Status_der)
print(paste('Accuracy',1-misClasificError_between_100_250))

########### Total Accuracy on whole data is 90.56 % #########################


#####################################################################################################
################## Analysis for Agenciew which sales is less than 100 Tractors  #####################
#####################################################################################################

agency_sale_less_than_100 <- filter(agg_agency_data, x < 100)
agency_sale_less_than_100

agency_sale_less_than_100_dtl <- subset (dis_mydata_subset, dis_mydata_subset$Dlr.Code == agency_sale_less_than_100$Category)
agency_sale_less_than_100_dtl



############################################################################################
###################### Splitting data between Train and Test ###############################

agency_sale_less_than_100_dtl

dt = sort(sample(nrow(agency_sale_less_than_100_dtl), nrow(agency_sale_less_than_100_dtl)*.7))
train_lessthan_100<-agency_sale_less_than_100_dtl[dt,]
test_lessthan_100<-agency_sale_less_than_100_dtl[-dt,]


model_less_than_100 <- glm (Status_der ~ .
                              , data = train_lessthan_100, family = binomial)
summary(model_less_than_100)

best_model_less_than_100 <- stepAIC (model_less_than_100, trace = FALSE)
best_model_less_than_100$anova

forward_model_less_than_100 <- stepAIC(model_less_than_100,direction = "forward", trace = FALSE)
forward_model_less_than_100$anova

backward_model_less_than_100 <- stepAIC(model_less_than_100,direction = "backward", trace = FALSE)
backward_model_less_than_100$anova


fitted.results_less_than_100 <- predict(best_model_less_than_100,newdata=test_total[,],type='response')
fitted.results_less_than_100 <- ifelse(fitted.results_less_than_100 > 0.5,1,0)
misClasificError_less_than_100 <- mean(fitted.results_less_than_100 != test_lessthan_100$Status_der)
print(paste('Accuracy',1-misClasificError_less_than_100))

############################### 92.99 % Accuracy ###################################




