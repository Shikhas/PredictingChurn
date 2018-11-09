#Load data
load_data <- function(num_memberrows,num_transrows,num_logsrows){
  train <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/train.csv'))
  members <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/members_v3.csv', nrows = num_memberrows))
  trans <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/transactions.csv', nrows = num_transrows))
  logs <- as.tibble(fread('~/Desktop/R_directory/PredictingChurn/data/user_logs.csv', nrows = num_logsrows))
}



#Observe content
observe_content <- function(x){
  summary(x)
  glimpse(x)
}

#Check missing values
check_missingvals <- function(x){
  sum(is.na(x)) 
}





