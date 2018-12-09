import::here(final_data, .from="dimensionality_reduction.R")

#Creating partition using createDataPartition method
index <- createDataPartition(final_data$sample.is_churn, p = 0.80, list = FALSE)
train_data <- final_data[index,]
test_data <- final_data[-index,]

train_data = train_data %>% select(-sample.msno)
test_data <- test_data %>% select(-sample.msno)
# change the sample size if you want.
train_data$sample.is_churn <- as.factor(train_data$sample.is_churn)
test_data$sample.is_churn <- as.factor(test_data$sample.is_churn)
#i <- sample(1:nrow(train_data), 5000)

train_data['class_names'] = ifelse(train_data$sample.is_churn == 1, "not_churned", "churned")
train_data = train_data %>% select(-sample.is_churn)

test_data['class_names'] = ifelse(test_data$sample.is_churn == 1, "not_churned", "churned")
test_data = test_data %>% select(-sample.is_churn)

################################################################################################
#Random Forest implementations
#class imbalance is kind of handled using sampling = 'rose' (can be changed to "up" or "down")
##"down" gives better specificity
trctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, sampling = "down", classProbs = TRUE)

set.seed(3456)

randf <- train(as.factor(class_names)~., data = train_data, method = "rf",
              trControl=trctrl1,
              preProcess = c("center", "scale"),
              tuneLength = 10)

###################################################################################################
#Test with the test set
#j <- sample(1:nrow(test_data), 1000)
test_pred <- predict(randf, newdata = test_data)

####################################################################################################
#Confusion matrix
F1_Score(test_data$sample.is_churn,test_pred,positive=NULL)
confusionMatrix(as.factor(test_pred), as.factor(test_data$sample.is_churn) )
cm <- confusionMatrix(as.factor(test_pred), as.factor(test_data$sample.is_churn) )
fourfoldplot(cm$table)

#####################################################################################################
#Random Forest OOB confusion matrix 
dd = randf$finalModel$confusion[,1:2]
rownames(dd) = c("Prediction: 1", "Prediction: 2")
colnames(dd) = c("Reference: 1", "Reference: 2")
fourfoldplot(dd)


#####################################################################################################
#Random Forest Variable importance plot 
random_forest_var_imp = randomForest(sample.is_churn ~., data = dd, importance=TRUE)
randomForest::varImpPlot(random_forest_var_imp)
randomForest::varImpPlot(random_forest_var_imp, type=2)
