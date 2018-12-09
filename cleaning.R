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
# check and remove duplicates

duplicate_train <- data_frame(duplicated = duplicated(train), row = 1:nrow(train)) %>% filter(duplicated == T)
duplicate_logs <- data_frame(duplicated = duplicated(logs), row = 1:nrow(logs)) %>% filter(duplicated == T)
duplicate_members <- data_frame(duplicated = duplicated(members), row = 1:nrow(members)) %>% filter(duplicated == T)
duplicate_trans <- data_frame(duplicated = duplicated(trans), row = 1:nrow(trans)) %>% filter(duplicated == T)

p1 = ggplot(duplicate_train, aes(xintercept = row)) +
  geom_vline(aes(xintercept = row)) + # plot a black line for each duplicated row
  ggtitle("Duplicated train data") + # add a title
  coord_flip() + scale_x_reverse() + xlab("row index")

p2 = ggplot(duplicate_logs, aes(xintercept = row)) +
  geom_vline(aes(xintercept = row)) + # plot a black line for each duplicated row
  ggtitle("Duplicated user logs data") + # add a title
  coord_flip() + scale_x_reverse() + xlab("row index")

p3 =  ggplot(duplicate_members, aes(xintercept = row)) +
  geom_vline(aes(xintercept = row)) + # plot a black line for each duplicated row
  ggtitle("Duplicated members data") + # add a title
  coord_flip() + scale_x_reverse() + xlab("row index")

p4 = ggplot(duplicate_trans, aes(xintercept = row)) +
  geom_vline(aes(xintercept = row)) + # plot a black line for each duplicated row
  ggtitle("Duplicated transactions data") + # add a title
  coord_flip() + scale_x_reverse() + xlab("row index")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)


trans = as.tibble(trans[-duplicate_trans$row, ])

rm("duplicate_train", "duplicate_trans", "duplicate_members", "duplicate_logs")  # remove not 
#needed variables from workspace

#################################################################################################
# Feature transformation

train <- train %>% mutate(is_churn = factor(is_churn))
members <- members %>% mutate(
  city = factor(city),  # converting to factor as unordered
  gender = factor(gender),  # converting to factor as unordered
  registered_via = factor(registered_via),
  registration_init_time = ymd(registration_init_time)
)

trans <- trans %>%  # dropping listed price as against actual price paid
  mutate(
    payment_method_id = factor(payment_method_id),  # converting to factor as unordered
    is_auto_renew = factor(is_auto_renew),  # converting to factor as unordered
    is_cancel = factor(is_cancel),  # converting to factor as unordered
    transaction_date = ymd(transaction_date),
    membership_expire_date = ymd(membership_expire_date)
  )

logs <- logs %>%
  mutate(date = ymd(date))

corrplot(cor(data.matrix(members %>% select(-msno, -gender))), method="pie")
corrplot(cor(data.matrix(logs %>% select(-msno))), method="pie")
corrplot(cor(data.matrix(trans %>% select(-msno))), method="pie")

p1 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_25)) + xlab("") + coord_flip()
p2 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_50)) + xlab("") + coord_flip()
p3 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_75)) + xlab("") + coord_flip()
p4 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_985)) + xlab("") + coord_flip()

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

p1 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_100)) + xlab("") + coord_flip()
p2 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=num_unq)) + xlab("") + coord_flip()
p3 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=total_secs)) + xlab("") + coord_flip()

layout <- matrix(c(1,2,3),3,1,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)

p1 = ggplot(data=members) + geom_boxplot(mapping=aes(x="", y=registration_init_time)) + xlab("") + ylab("members$registration_init_time") + coord_flip()
p2 = ggplot(data=logs) + geom_boxplot(mapping=aes(x="", y=date)) + xlab("") + ylab("logs$date") + coord_flip()
layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)

p1 = ggplot(data=trans) + geom_boxplot(mapping=aes(x="", y=actual_amount_paid)) + xlab("") + ylab("transactions$actual_amount_paid") + coord_flip()
p2 = ggplot(data=trans) + geom_boxplot(mapping=aes(x="", y=plan_list_price)) + xlab("") + ylab("transactions$plan_list_price") + coord_flip()
layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)

members = members %>% filter(bd>=0, bd<=100) %>% filter(registration_init_time>=20080000)
logs = logs %>% filter(num_25>=0, num_25<=1000) %>% filter(num_50>=0, num_50<=500) %>%
  filter(num_75>=0, num_75<=200) %>% filter(num_985>=0, num_985<=500) %>% filter(num_100>=0, num_100<=5000) %>%
  filter(num_unq>=0, num_unq<=1000) %>% filter(total_secs>=-2.5e+15, total_secs<=2.5e+15)
trans = trans %>% filter(actual_amount_paid>=0, actual_amount_paid<=1000) %>% filter(plan_list_price>=0, plan_list_price<=1000)

#################################################################################################
# sanity checks

train = train %>% dplyr::filter(nchar(msno) == 44) %>% dplyr::filter(endsWith(msno, '='))

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

corrplot(cor(data.matrix(members %>% select(-msno, -gender))), method="pie")
corrplot(cor(data.matrix(logs %>% select(-msno))), method="pie")
corrplot(cor(data.matrix(trans %>% select(-msno))), method="pie")

trans <- trans %>% select(-plan_list_price)

#################################################################################################
# Keep only users present in train dataset

trans = dplyr::filter(trans, msno %in% train$msno)
logs = dplyr::filter(logs, msno %in% train$msno)
members = dplyr::filter(members, msno %in% train$msno)


sample = inner_join(train, members, by= ("msno"="msno")) %>% 
  inner_join(trans, by=c("msno"="msno")) %>% 
  inner_join(logs, by=c("msno"="msno"))

sample = droplevels(sample)
sample = distinct(sample)
