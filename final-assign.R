library(caret); library(rattle); library(rpart); library(rpart.plot)
library(randomForest); library(repmis)
#import data and load data from local file
training <- read.csv("C:\\projects\\practicleML\\pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("C:\\projects\\practicleML\\pml-testing.csv", na.strings = c("NA", ""))
#remove /null/empty values
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

#remove first seven columns/predictors as the does not impact predictions
training_data <- training[, -c(1:7)]
test_data <- testing[, -c(1:7)]
set.seed(7826) 
inTrain <- createDataPartition(training_data$classe, p = 0.7, list = FALSE)
train <- training_data[inTrain, ]
valid <- training_data[-inTrain, ]
control <- trainControl(method = "cv", number = 5)
fit_rpart <- train(classe ~ ., data = train, method = "rpart", trControl = control)
print(fit_rpart, digits = 4)
fancyRpartPlot(fit_rpart$finalModel)
#get the predicton
predict_rpart <- predict(fit_rpart, valid)
(confusion_matpart <- confusionMatrix(valid$classe, predict_rpart))
(accuracy_rpart <- confusion_matpart$overall[1])
fit_rf <- train(classe ~ ., data = train, method = "rf", trControl = control)
print(fit_rf, digits = 4)
predict_rf <- predict(fit_rf, valid)
(conf_rf <- confusionMatrix(valid$classe, predict_rf));
(accuracy_rf <- conf_rf$overall[1]);


