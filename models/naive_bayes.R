import::here(final_data, .from="dimensionality_reduction.R")

#Creating partition using createDataPartition method
index <- createDataPartition(final_data$sample.is_churn, p = 0.80, list = FALSE)
train_data <- final_data[index,]
test_data <- final_data[-index,]

# change the sample size if you want.
train_data$sample.is_churn <- as.factor(train_data$sample.is_churn)
test_data$sample.is_churn <- as.factor(test_data$sample.is_churn)
i <- sample(1:nrow(train_data), 5000)

############################################################
#Random Forest implementations
#class imbalance is kind of handled using sampling = 'rose' (can be changed to "up" or "down")
##"down" gives better specificity
trctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3, sampling = "down")

set.seed(3456)

naiveb <- train(sample.is_churn~., data = train_data[i,], method = "nb",
               trControl=trctrl1(method='cv',number=10),
               preProcess = c("center", "scale"),
               tuneLength = 10)

##Test with the test set
j <- sample(1:nrow(test_data), 1000)
test_pred <- predict(naiveb, newdata = test_data[j,])

##Confusion matrix
F1_Score(test_data[j,]$sample.is_churn,test_pred,positive=NULL)
confusionMatrix(test_pred, test_data[j,]$sample.is_churn )

