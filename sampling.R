#Load data
train <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/train.csv'))
members <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/members_v3.csv', nrows = 1e6))
trans <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/transactions.csv', nrows = 1e6))
logs <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/churn_comp_refresh/user_logs_v2.csv', nrows = 5e6))

#Observe content
summary(train)
glimpse(train) #see glimpse or every column in tibble
summary(members)
glimpse(members)
summary(trans)
glimpse(trans)
summary(logs)
glimpse(logs)

#Check missing values 
sum(is.na(train)) 
sum(is.na(members))
sum(is.na(trans))
sum(is.na(logs))






