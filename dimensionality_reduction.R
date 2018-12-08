import::here(sample, .from="cleaning.R")

# Convert non-numeric values to numeric

sample[] <- lapply(sample, function(x) {
  if(is.character(x))
    as.numeric(as.factor(x))
  else if(is.factor(x) || is.integer(x) || is.Date(x))
    as.numeric(x)
  else
    x
})


#Performing PCA using factominer for quick analysis.
res = PCA(sample[,3:22], scale.unit = TRUE,graph = FALSE)
eig.val <- get_eigenvalue(res)
eig.val
res$eig[1:15,] 

#####################
# 15 components have larger eigenvalues than the rest. 
# We can retain these 15 components and ignore the rest with zero or near zero values.

# Scree plot
fviz_eig(res, addlabels = TRUE, ylim = c(0, 20), ncp=20)
#####################
#Using prcomp() instead to extract the principle components into the dataset.
res.pca = prcomp(sample[,3:22], scale = TRUE)
#####################
#Just to make sure that the results are same. Can skip this part. 
# Eigenvalues
eig <- (res.pca$sdev)^2

# Variances in percentage
variance <- eig*100/sum(eig)

# Cumulative variances
cumvar <- cumsum(variance)

eig.sample <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
eig.sample

#Final dataset with 17 variables.
df1 = data.frame(sample$msno,sample$is_churn)
df2 = res.pca$x[,1:11]
final_data = cbind(df1,df2)
