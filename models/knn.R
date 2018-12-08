setwd("/Users/shama/Assignments/dpa_project/PredictingChurn-master")
import::here(final_data, .from="dimensionality_reduction.R")



#Creating partition using createDataPartition method
index <- createDataPartition(final_data$sample.is_churn, p = 0.80, list = FALSE)
train_data <- final_data[index,]
test_data <- final_data[-index,]

# change the sample size if you want.
train_data$sample.is_churn <- as.factor(train_data$sample.is_churn)
i <- sample(1:nrow(train_data), 5000)
#KNN implementation

#Use the entire train_data to fit the model (takes longer, hence tested the model with sample data)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, sampling='rose')
set.seed(3456)
knn_fit <- train(sample.is_churn~., data = train_data[i,], method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit 
plot(knn_fit)

#test on train set
j <- sample(1:nrow(test_data), 1000)
test_pred <- predict(knn_fit, newdata = test_data[j,])
test_pred

#Confusion matrix
F1_Score(test_data[j,]$sample.is_churn,test_pred,positive=NULL)
cm <- confusionMatrix(test_pred, test_data[j,]$sample.is_churn)
fourfoldplot(cm$table)
