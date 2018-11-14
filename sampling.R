#Load data
load_data <- function(num_memberrows,num_transrows,num_logsrows){
  train <- as.tibble(fread('data/train.csv'))
  members <- as.tibble(fread('data/members_v3.csv', nrows = num_memberrows))
  trans <- as.tibble(fread('data/transactions.csv', nrows = num_transrows))
  logs <- as.tibble(fread('data/user_logs.csv', nrows = num_logsrows))
  return(list("train"=train, "members"=members, "trans"=trans, "logs"=logs))
}


data_list = load_data(1e6,1e6,5e6)

train = data_list$train
members = data_list$members
trans = data_list$trans
logs = data_list$logs

#Observe content
observe_content <- function(x){
  summary(x)
  glimpse(x)
}

#Check missing values
check_missingvals <- function(x){
  sum(is.na(x)) 
}





