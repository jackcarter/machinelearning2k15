#setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

#### predictive modeling

source("GetData.R")

library(randomForest)

m <- round(sqrt(ncol(df_smtrain)))
n_tree <- 50

# baseline RF to get a sense of important variables
mod_rf <- randomForest(status_group ~ ., data=df_smtrain, mtry=m, ntree=n_tree)
varImpPlot(mod_rf)

# out-of-bag: 23.1% misclassification rate
pred_mod_rf_oob <- predict(mod_rf)  # with newdata unspecified, uses OOB
misclass_mod_rf_oob <- sum(pred_mod_rf_oob != df_smtrain[, "status_group"]) / nrow(df_smtrain)
cat("OOB misclassification rate: ", misclass_mod_rf_oob)

# new data
pred_mod_rf_newdata <- predict(mod_rf, newdata=df_test)
misclass_mod_rf_newdata <- sum(pred_mod_rf_newdata != df_test[, "status_group"]) / nrow(df_test)
cat("newdata misclassification rate: ", misclass_mod_rf_newdata)


# more trees, bigger data set
m <- round(sqrt(ncol(df_train)))
n_tree <- 1000
mod_rf2 <- randomForest(status_group ~ ., data=df_train, mtry=m, ntree=n_tree)
varImpPlot(mod_rf2)

# out-of-bag: 23.1% misclassification rate
pred_mod_rf2_oob <- predict(mod_rf2)  # with newdata unspecified, uses OOB
misclass_mod_rf2_oob <- sum(pred_mod_rf2_oob != df_train[, "status_group"]) / nrow(df_train)
cat("OOB misclassification rate for rf2: ", misclass_mod_rf2_oob)

# new data
pred_mod_rf2_newdata <- predict(mod_rf2, newdata=df_test)
misclass_mod_rf2_newdata <- sum(pred_mod_rf2_newdata != df_test[, "status_group"]) / nrow(df_test)
cat("newdata misclassification rate for rf2: ", misclass_mod_rf2_newdata)


####
#### testing to figure out optimum mtry and ntree
#### mtry=4, ntree=1000 performed best 
####
# results
#misclassification_rate = list()

# ##settings for randomForest
# p <- ncol(df_smtrain) - 1
# mtryv = c(p, floor(sqrt(p)))
# ntreev = c(500,1000)
# setrf = expand.grid(mtryv,ntreev)
# colnames(setrf)=c("mtry","ntree")

###fit rf
# m=4, n_tree=1000 performed best
# 
# for(i in 1:nrow(setrf)) {
#   cat("on randomForest fit ",i,"\n")
#   print(setrf[i,])
#   
#   #fit and predict
#   mod_rf_tmp = randomForest(status_group ~ ., data=df_smtrain, mtry=setrf[i,1], ntree=setrf[i,2])
#   pred_tmp <- predict(mod_rf_tmp)  # with newdata unspecified, uses OOB
#   misclass_tmp <- sum(pred_tmp != df_smtrain[, "status_group"]) / nrow(df_smtrain)
#   
#   cat(misclass_tmp)
# }

