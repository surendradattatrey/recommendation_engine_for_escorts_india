library(ggplot2)
library(tree)
library(randomForest)
library(caret)
library(ROCR)
library(MASS)
library(rpart)
library(e1071)
library(rpart.plot)

################################################################################
################## Decision Tree ###############################################

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(Status_der ~ ., data = train_total, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred <- predict(dtree_fit, newdata = test_total)
confusionMatrix(test_pred, test_total$Status_der )  #check accuracy

####################### Decision Tree Gini Index ##############################

set.seed(3333)
dtree_fit_gini <- train(Status_der ~ ., data = train_total, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                          tuneLength = 10)
dtree_fit_gini

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

###########################################################################################
################################# Random Forest ###########################################
#########################################################################################

rf <-randomForest(Status_der ~ ., data=train_total, ntree=1000) 
print(rf)

predTrain <- predict(rf, train_total, type = "class")
table(predTrain, train_total$Status_der) ## Train Confusion Matrix

predValid <- predict(rf, test_total, type = "class")
mean(predValid == test_total$Status_der)                    
table(predValid,test_total$Status_der)

importance(rf)        
varImpPlot(rf)  

