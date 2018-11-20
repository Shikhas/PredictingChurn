import::here(train, trans, members, logs, .from="sampling.R")

#################################################################################################
# NA value detection and removal: works awesome for int type

sum(is.na(train))
sum(is.na(members))
sum(is.na(trans))
sum(is.na(logs))  # All are zero

if (FALSE){  # this block is skipped: we have no NA's in our data, YAYYY!!
  na.omit(train)
  na.omit(logs)
  na.omit(members)
  na.omit(trans)
}
#################################################################################################
# sanity checks

train = train %>% dplyr::filter(nchar(msno) == 44) %>% dplyr::filter(endsWith(msno, '='))

#################################################################################################
# check and remove duplicates
duplicated_train <- duplicated(train)
duplicated_trans <- duplicated(trans)
duplicated_members <- duplicated(members)
duplicated_logs <-  duplicated(logs)

if (any(duplicated_train == TRUE)){
  train = as.tibble(train[-duplicated_train, ])
}

if (any(duplicated_logs == TRUE)){
  logs = as.tibble(logs[-duplicated_logs, ])
}

if (any(duplicated_members == TRUE)){
  members = as.tibble(members[-duplicated_members, ])
}

if (any(duplicated_trans == TRUE)){
  trans = as.tibble(trans[-duplicated_trans, ])
}

rm("duplicated_train", "duplicated_trans", "duplicated_members", "duplicated_logs")  # remove not 
#needed variables from workspace

#################################################################################################
# Keep only users present in train dataset

trans = dplyr::filter(trans, msno %in% train$msno)
logs = dplyr::filter(logs, msno %in% train$msno)
members = dplyr::filter(members, msno %in% train$msno)


#################################################################################################
# Feature transformation

train <- train %>% mutate(is_churn = factor(is_churn))
members <- members %>% mutate(
  city = factor(city),  # converting to factor as unordered
  gender = factor(gender),  # converting to factor as unordered
  registered_via = factor(registered_via),
  registration_init_time = ymd(registration_init_time)
)

trans <- trans %>% select(-plan_list_price) %>%  # dropping listed price as against actual price paid
  mutate(
    payment_method_id = factor(payment_method_id),  # converting to factor as unordered
    is_auto_renew = factor(is_auto_renew),  # converting to factor as unordered
    is_cancel = factor(is_cancel),  # converting to factor as unordered
    transaction_date = ymd(transaction_date),
    membership_expire_date = ymd(membership_expire_date)
  )

logs <- logs %>%
  mutate(date = ymd(date))

#################################################################################################
# Missing value imputation

# Members data has got gender missing values
# imputing it using KNN 
knn_impute_missing_gender_from_members = function(){
  has_empty = members %>% dplyr::filter(gender == "")
  has_non_empty = members %>% dplyr::filter(gender != "")
  train_sample = has_non_empty %>% select(-"msno") %>% select(-"gender")
  test_sample = has_empty %>% select(-"msno") %>% select(-"gender")
  gender_knn_pred = class::knn(data.matrix(train_sample), data.matrix(test_sample), has_non_empty$gender, k=5)  # data.matrix to 
  # turn features into numeric
  has_empty$gender = gender_knn_pred
  return(bind_rows(has_empty, has_non_empty))
}

members = knn_impute_missing_gender_from_members()

sample = inner_join(train, members, by= ("msno"="msno")) %>% 
  inner_join(trans, by=c("msno"="msno")) %>% 
  inner_join(logs, by=c("msno"="msno"))

