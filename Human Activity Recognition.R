#IMPORT LIBRARIES AND DATA####

#Removing pre-existing variables from workspace
rm(list = ls())

#Set Seed
set.seed(42)

#Initializing Libraries 
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(repmis)

#Importing Data
trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
validurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train <- source_data(trainurl, na.strings = c("NA", "#DIV/0!", ""), header = TRUE)
valid <- source_data(validurl, na.strings = c("NA", "#DIV/0!", ""), header = TRUE)

#DATA TREATMENT####

#Remove Columns with > 80% NAs
train <- train[, colSums(is.na(train)) < 0.8*nrow(train)]
valid <- valid[, colSums(is.na(valid)) < 0.8*nrow(valid)]

#Remove Redundant Columns
train <- train[, -c(1:7)]
valid <- valid[, -c(1:7)]

#TRAIN - TEST SPLIT####
inTrain <- createDataPartition(train$classe, p = 0.8, list = FALSE)
test <- train[-inTrain,]
train <- train[inTrain,]

#MODEL####

#Resampling Method
control <- trainControl("cv")

#Build Model
model <- train(classe ~ ., data = train, method = "rf", trControl = trainControl("cv"))

#View Model Details
print(model, digits = 4)

#PREDICT####
predicted_t <- predict(model, test)
predicted_v <- predict(model, valid)

#20 Observation Unknown Dataset
prediction <- cbind(problem_id = valid$problem_id, prediction = as.character(predicted_v))

#EVALUATE MODEL####

#Confusion Matrix
confusionMatrix(as.factor(test$classe), predicted_t)