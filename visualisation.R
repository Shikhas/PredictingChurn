import::here(sample, .from = "cleaning.R")
#########################################################################################
# Feature visualization and related plots


#########################################################################################
# Mutiplot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##########################################################################################
# Observing members and train data numerically 
sample %>% count(gender)
# more female than male by this count results

sample %>%
group_by(is_churn) %>%
summarise(percentage = n()/nrow(sample)*100)
# only around 3.75% users churned, resulting in imbalanced classification

#########################################################################################
# Visualizing train and members data
p1 <- sample %>%
ggplot(aes(is_churn, fill = is_churn)) + geom_bar() + theme(legend.position = "none")

p2 <- sample %>%
ggplot(aes(gender, fill = gender)) + geom_bar() + theme(legend.position = "none")

p3 <- sample %>%
ggplot(aes(registered_via, fill = registered_via)) + geom_bar() + theme(legend.position = "none") + scale_y_sqrt()

p4 <- sample %>% 
ggplot(aes(city, fill = city)) + geom_bar() + theme(legend.position = "none") + scale_y_sqrt()

p5 <- sample %>%
filter(bd > 0 & bd < 100) %>% ggplot(aes(bd)) + geom_density(fill = "blue", bw = 1)

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)

###########################################################################################
# Visualizing dates of initial registration

# frequecncy polygon of initial registration is created using geom_freqpoly in order to observe popularity rise over the years
# The frequency ploygon shows that popularity slowly rising after 2010 and strongly increased near 2015
p1 <- sample %>%
ggplot(aes(registration_init_time)) + geom_freqpoly(color = "red", binwidth = 1) 


# Number of registration  observed against weekdays using Lubricate::Wday with weekday labelled as TRUE
# This tells us that few days of week have higher registration numbers than other days
p2 <- sample %>%
mutate(wday = wday(registration_init_time, label = TRUE)) %>% ggplot(aes(wday, fill = wday)) +
geom_bar() + theme(legend.position = "none") + labs(x = "Day of the week")

# The interval to filter registration, is from 2004-12-31 to 2017-01-01 and month function is used to observe number of registration
# over the months of the year as useful information is 2015 onward
p3 <- sample %>%
filter(registration_init_time > ymd("20041231") & registration_init_time < ymd("20170101")) %>%
mutate(month = month(registration_init_time , label = TRUE)) %>%
ggplot(aes(month, fill = month)) +
geom_bar() +
theme(legend.position = "none") +
labs(x = "Month of the year")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)

###############################################################################################
# Visualizing expiration dates

# Below tells us the years to filter and focus in order to observe the expiration dates
sample %>%
ggplot(aes(membership_expire_date)) + geom_freqpoly(color = "blue", binwidth = 5) + facet_zoom(x = (membership_expire_date > ymd("20140901") & membership_expire_date < ymd("20180301")))

# We focus between 2014 and 2018 as pattern was observed only for these years
# This tells us that few days of week have higher expiration numbers than other days
p1 <- sample %>%
filter(membership_expire_date > ymd("20140901") & membership_expire_date < ymd("20180301")) %>% mutate(wday = wday(membership_expire_date, label = TRUE)) %>%
ggplot(aes(wday, fill = wday)) + geom_bar() + theme(legend.position = "none") + labs(x = "Day of the week")

p2 <- sample %>%
filter(membership_expire_date > ymd("20140901") & membership_expire_date < ymd("20180301")) %>% mutate(month = month(membership_expire_date, label = TRUE)) %>%
ggplot(aes(month, fill = month)) + geom_bar() + theme(legend.position = "none") + labs(x = "Month of the year")

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1,p2, layout=layout)

###############################################################################################
# Visualizing payment and membership related information 

# This will show which payment methods are more popular
p1 <- sample %>%
ggplot(aes(payment_method_id, fill = payment_method_id)) +
geom_bar() + scale_y_sqrt() + theme(legend.position = "none") + labs(x = "Payment method")

# This shows proportion of population having automatic renewal of their subscriptions enabled
p2 <- sample %>%
ggplot(aes(is_auto_renew, fill = is_auto_renew)) + geom_bar() + theme(legend.position = "none")

# This shows proportion of users who actively cancelled their subscriptions
p3 <- sample %>%
ggplot(aes(is_cancel, fill = is_cancel)) + geom_bar() + theme(legend.position = "none")

# This tells the payment plan duration plot in days
p4 <- trans %>%
mutate(payment_plan_days = factor(payment_plan_days)) %>%
ggplot(aes(payment_plan_days, fill = payment_plan_days)) + geom_bar() + scale_y_sqrt() + theme(legend.position = "none") +
labs(x = "Payment plan duration (days)")

layout <- matrix(c(1,1,2,3,4,4),3,2,byrow=TRUE)
multiplot(p1,p2,p3,p4, layout = layout)

#################################################################################################
# Visualizing the user logs

# This shows entries (=days) per user in the sample
p1 <- sample %>%
count(msno) %>% ggplot(aes(n)) + geom_bar(fill = "red") + labs(x = "Entries per user")

# This shows that total listening time per day per user(total_secs) is log-normal distributed
# geom_vline will draw a vertical line at median value of total seconds
# scale_x_log10 - log transformation of given value - takes log of x value
p2 <- sample %>%
filter(abs(total_secs)<1e5) %>% ggplot(aes(total_secs)) + geom_vline(xintercept = median(sample$total_secs), linetype = 2) +
geom_density(fill = "green", alpha = 0.5) + scale_x_log10()

# This shows pattern for the number of unique songs per day (num_unq) listened by user which shows listereners with more than 100 unique songs are rare
# geom_vline will draw a vertical line at median value of num_unq
# scale_x_log10 - log transformation to draw logarithmic plot
p3 <- sample %>%
ggplot(aes(num_unq)) + geom_vline(xintercept = median(sample$num_unq), linetype = 2) +
geom_histogram(binwidth = .05, fill = "blue", alpha = 0.7) + scale_x_log10()

# This shows density plot for percentage of song completion by listeners 
p4 <- sample %>%
gather(num_25, num_50, num_75, num_985, num_100, key = "slen", value = "cases") %>%
mutate(slen = fct_relevel(factor(slen),"num_100", after = Inf)) %>% ggplot(aes(cases, fill = slen)) +
geom_density(position = "stack", bw = .1) + scale_x_log10(lim = c(1,800)) +
labs(x = "Number of songs", fill = "% played")

layout = matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1,p2,p3,p4,layout = layout)

# This plot shows number of logs against time(date)
p1 <- sample %>%
count(date) %>% ggplot(aes(date,n)) + geom_line(color = "green")
plot(p1)

##################################################################################################
# Visualize various features against churn count

# visualize the proportion of users who actively cancelled among churn and no_churn users
p1 <- sample %>% ggplot(aes(is_churn, fill = is_cancel)) + geom_bar(mapping=aes(group=is_cancel),position="fill") 

# visualize the proportion of is_auto_renew enabled users among churn and no_churn users
p2 <- sample %>%
ggplot(aes(is_churn, fill = is_auto_renew)) + geom_bar(mapping=aes(group=is_auto_renew),position="fill") 

layout <- matrix(c(1,2),1,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)



