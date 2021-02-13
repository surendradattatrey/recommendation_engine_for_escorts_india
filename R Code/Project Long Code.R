rm(list = ls())

library(data.table)
library(dplyr)
library(pscl)
library(caret)
library(pscl)
library(ROCR)

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

model_1 <- glm (Status_der ~ Dlr.Code, data = dis_mydata_subset, family = binomial)
summary(model_1)

model_2 <- glm (Status_der ~ Dlr.Code + Tehsil, data = dis_mydata_subset, family = binomial)
summary(model_2)

model_3 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired, data = dis_mydata_subset, family = binomial)
summary(model_3)

model_4 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE, data = dis_mydata_subset, family = binomial)
summary(model_4)

model_5 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type , 
                data = dis_mydata_subset, family = binomial)
summary(model_5)

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

############################################################################################
###################### Splitting data between Train and Test ###############################

dt = sort(sample(nrow(dis_mydata_subset), nrow(dis_mydata_subset)*.7))
train_total<-dis_mydata_subset[dt,]
test_total<-dis_mydata_subset[-dt,]


## Model 1 - With all variables
model <- glm (Status_der ~ ., data = train_total, family = binomial)
summary(model)

model_1 <- glm (Status_der ~ Dlr.Code, data = train_total, family = binomial)
summary(model_1)

model_2 <- glm (Status_der ~ Dlr.Code + Tehsil, data = train_total, family = binomial)
summary(model_2)

model_3 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired, data = train_total, family = binomial)
summary(model_3)

model_4 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE, data = train_total, family = binomial)
summary(model_4)

model_5 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type , 
                data = train_total, family = binomial)
summary(model_5)

model_6 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + Tehsil*Enq.Type , 
                data = train_total, family = binomial)
summary(model_6)

anova(model, test="Chisq")
anova(model_1, test="Chisq")
anova(model_2, test="Chisq")
anova(model_3, test="Chisq")
anova(model_4, test="Chisq")
anova(model_5, test="Chisq")
anova(model_6, test="Chisq")


fitted.results <- predict(model,newdata=test_total[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_total$Status_der)
print(paste('Accuracy',1-misClasificError))

########### Total Accuracy on whole data is 92.43% #########################


####################################################################################################
############       Segeregating data based on dealer Category      #################################
####################################################################################################

agg_agency_data <- aggregate(dis_mydata_subset$Status_der, 
                             by=list(Category=dis_mydata_subset$Dlr.Code), FUN=sum)
agg_agency_data

agency_sale_more_than_250 <- filter(agg_agency_data, x > 250)
agency_sale_more_than_250

agency_sale_more_than_250_dtl <- subset (dis_mydata_subset, dis_mydata_subset$Dlr.Code == agency_sale_more_than_250$Category)
agency_sale_more_than_250_dtl


##################################################################################################
################## Analysis for Agenciew which sales is more than 250 Tractors ###################
##################################################################################################

model_dlr_more_than_250 <- glm (Status_der ~ ., data = agency_sale_more_than_250_dtl, family = binomial)
summary(model_dlr_more_than_250)

## Model 2 - without Dealer Code variables
model_dlr_more_than_250_dlr_code <- glm (Status_der ~ .-Dlr.Code, data = agency_sale_more_than_250_dtl, family = binomial(link="logit"))
summary(model_dlr_more_than_250_dlr_code)

## Model 3 with Traction effect of DSE and Tehsil
model_dlr_more_than_250_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                                         , data = agency_sale_more_than_250_dtl, family = binomial(link="logit"))
summary(model_dlr_more_than_250_dlr_code)

## Model 4 with Traction effect of DSE and Model Enquired
model_dlr_more_than_250_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Model.Enquired
                                         , data = agency_sale_more_than_250_dtl, family = binomial(link="logit"))
summary(model_dlr_more_than_250_dlr_code)

## Model 5 other Trial

agency_sale_more_than_250_dtl_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                                             , data = agency_sale_more_than_250_dtl, family = binomial(link="logit"))
summary(agency_sale_more_than_250_dtl_wo_dlr)

agency_sale_more_than_250_dtl

############################################################################################
###################### Splitting data between Train and Test ###############################

dt = sort(sample(nrow(agency_sale_more_than_250_dtl), nrow(agency_sale_more_than_250_dtl)*.7))
train_morethan_250<-agency_sale_more_than_250_dtl[dt,]
test_morethan_250<-agency_sale_more_than_250_dtl[-dt,]


## Model 1 - With all variables
model_250 <- glm (Status_der ~ ., data = train_morethan_250, family = binomial)
summary(model_250)

model_250_1 <- glm (Status_der ~ Dlr.Code, data = train_morethan_250, family = binomial)
summary(model_1)

model_250_2 <- glm (Status_der ~ Dlr.Code + Tehsil, data = train_morethan_250, family = binomial)
summary(model_250_1)

model_250_3 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired, data = train_morethan_250, family = binomial)
summary(model_250_3)

model_250_4 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE, data = train_morethan_250, family = binomial)
summary(model_250_4)

model_250_5 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type , 
                    data = train_morethan_250, family = binomial)
summary(model_250_5)

model_250_6 <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + Tehsil*Enq.Type , 
                    data = train_morethan_250, family = binomial)
summary(model_250_6)

anova(model_250, test="Chisq")
anova(model_250_1, test="Chisq")
anova(model_250_2, test="Chisq")
anova(model_250_3, test="Chisq")
anova(model_250_4, test="Chisq")
anova(model_250_5, test="Chisq")
anova(model_250_6, test="Chisq")


fitted.results <- predict(model_250,newdata=test_morethan_250[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_morethan_250$Status_der)
print(paste('Accuracy',1-misClasificError))

########### Total Accuracy on whole data is 90.20% #########################

###############################################################################################
####################### Sales beween 100 to 250 ###############################################
###############################################################################################

agency_sale_between_100_to_250 <- filter(agg_agency_data, x < 250 & x > 100)
agency_sale_between_100_to_250

agency_sale_between_100_to_250_dtl <- subset (dis_mydata_subset, dis_mydata_subset$Dlr.Code == agency_sale_between_100_to_250$Category)
agency_sale_between_100_to_250_dtl


model_dlr_between_100_and_250 <- glm (Status_der ~ ., data = agency_sale_between_100_to_250_dtl, family = binomial)
summary(model_dlr_between_100_and_250)

## Model 2 - without Dealer Code variables
agency_sale_between_100_to_250_wo_dtl_dlr_code <- glm (Status_der ~ .-Dlr.Code, data = agency_sale_between_100_to_250_dtl, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_wo_dtl_dlr_code)

## Model 3 with Traction effect of DSE and Tehsil
agency_sale_between_100_to_250_dtl_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                                                    , data = agency_sale_between_100_to_250_dtl, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code)

## Model 4 with Traction effect of DSE and Model Enquired
agency_sale_between_100_to_250_dtl_dlr_code_traction <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Model.Enquired
                                                             , data = agency_sale_between_100_to_250_dtl, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code_traction)

## Model 5 other Trial

agency_sale_between_100_to_250_dtl_dlr_code_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                                                           , data = agency_sale_between_100_to_250_dtl, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code_wo_dlr)


############################################################################################
###################### Splitting data between Train and Test ###############################


agency_sale_between_100_to_250_dtl

dt_between_100_250 = sort(sample(nrow(agency_sale_between_100_to_250_dtl), nrow(agency_sale_between_100_to_250_dtl)*.7))
train_between_100_250<-agency_sale_between_100_to_250_dtl[dt_between_100_250,]
test_between_100_250<-agency_sale_between_100_to_250_dtl[-dt_between_100_250,]

agency_sale_between_100_to_250_dtl


model_dlr_between_100_and_250 <- glm (Status_der ~ ., data = train_between_100_250, family = binomial)
summary(model_dlr_between_100_and_250)

## Model 2 - without Dealer Code variables
agency_sale_between_100_to_250_wo_dtl_dlr_code <- glm (Status_der ~ .-Dlr.Code, data = train_between_100_250, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_wo_dtl_dlr_code)

## Model 3 with Traction effect of DSE and Tehsil
agency_sale_between_100_to_250_dtl_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                                                    , data = train_between_100_250, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code)

## Model 4 with Traction effect of DSE and Model Enquired
agency_sale_between_100_to_250_dtl_dlr_code_traction <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Model.Enquired
                                                             , data = train_between_100_250, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code_traction)

## Model 5 other Trial

agency_sale_between_100_to_250_dtl_dlr_code_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                                                           , data = train_between_100_250, family = binomial(link="logit"))
summary(agency_sale_between_100_to_250_dtl_dlr_code_wo_dlr)


fitted.results <- predict(model_dlr_between_100_and_250,newdata=train_between_100_250[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_between_100_250$Status_der)
print(paste('Accuracy',1-misClasificError))

########### Total Accuracy on whole data is 90.56 % #########################


#####################################################################################################
################## Analysis for Agenciew which sales is less than 100 Tractors  #####################
#####################################################################################################

agency_sale_less_than_100 <- filter(agg_agency_data, x < 100)
agency_sale_less_than_100

agency_sale_less_than_100_dtl <- subset (dis_mydata_subset, dis_mydata_subset$Dlr.Code == agency_sale_less_than_100$Category)
agency_sale_less_than_100_dtl


model_dlr_less_than_100 <- glm (Status_der ~ ., 
                                data = agency_sale_less_than_100_dtl, family = binomial)
summary(model_dlr_less_than_100)



## Model 2 - without Dealer Code variables
agency_sale_less_than_100_wo_dtl_dlr_code <- glm (Status_der ~ .-Dlr.Code, 
                                                  data = agency_sale_less_than_100_dtl, 
                                                  family = binomial(link="logit"))
summary(agency_sale_less_than_100_wo_dtl_dlr_code)

## Model 3 with Traction effect of DSE and Tehsil
agency_sale_less_than_100_dtl_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                                               , data = agency_sale_less_than_100_dtl, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code)

## Model 4 with Traction effect of DSE and Model Enquired
agency_sale_less_than_100_dtl_dlr_code_traction <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Model.Enquired
                                                        , data = agency_sale_less_than_100_dtl, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code_traction)

## Model 5 other Trial

agency_sale_less_than_100_dtl_dlr_code_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                                                      , data = agency_sale_less_than_100_dtl, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code_wo_dlr)

############################################################################################
###################### Splitting data between Train and Test ###############################

agency_sale_less_than_100_dtl

dt = sort(sample(nrow(agency_sale_less_than_100_dtl), nrow(agency_sale_less_than_100_dtl)*.7))
train_lessthan_100<-agency_sale_less_than_100_dtl[dt,]
test_lessthan_100<-agency_sale_less_than_100_dtl[-dt,]

model_dlr_less_than_100 <- glm (Status_der ~ ., 
                                data = train_lessthan_100, family = binomial)
summary(model_dlr_less_than_100)



## Model 2 - without Dealer Code variables
agency_sale_less_than_100_wo_dtl_dlr_code <- glm (Status_der ~ .-Dlr.Code, 
                                                  data = train_lessthan_100, 
                                                  family = binomial(link="logit"))
summary(agency_sale_less_than_100_wo_dtl_dlr_code)

## Model 3 with Traction effect of DSE and Tehsil
agency_sale_less_than_100_dtl_dlr_code <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Tehsil
                                               , data = train_lessthan_100, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code)

## Model 4 with Traction effect of DSE and Model Enquired
agency_sale_less_than_100_dtl_dlr_code_traction <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired + DSE + Enq.Type + DSE*Model.Enquired
                                                        , data = train_lessthan_100, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code_traction)

## Model 5 other Trial

agency_sale_less_than_100_dtl_dlr_code_wo_dlr <- glm (Status_der ~ Dlr.Code + Tehsil + Model.Enquired  + Enq.Type
                                                      , data = train_lessthan_100, family = binomial(link="logit"))
summary(agency_sale_less_than_100_dtl_dlr_code_wo_dlr)


fitted.results <- predict(model_dlr_less_than_100,newdata=test_lessthan_100[,],type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_lessthan_100$Status_der)
print(paste('Accuracy',1-misClasificError))

############################### 92.99 % Accuracy ###################################




