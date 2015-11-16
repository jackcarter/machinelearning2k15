#setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

#### Random Forests

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
m <- 4
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


########## evaluate mod_rf2

groups <- c("functional", "functional needs repair", "non functional")


## false positive / false negative
pred_correct <- pred_mod_rf2_newdata == df_test[, "status_group"]

for (i in 1:length(groups)) {
  group <- groups[i]
  total <- sum(df_test$status_group == group)
  correct <- sum(pred_mod_rf2_newdata == group & df_test$status_group == group)
  false_pos <- sum(pred_mod_rf2_newdata == group & df_test$status_group != group)
  false_neg <- sum(pred_mod_rf2_newdata != group & df_test$status_group == group)
  cat(group, "\n")
  cat("  total:", total)
  cat("  correct:", correct)
  cat("  false positive:", false_pos)
  cat("  false negative:", false_neg)
  cat("\n\n")
}


#### plot model accuracy on the map

library(ggplot2)
library(scales)
library(ggmap)
library(reshape2)

##map fun
map <- get_map("Tanzania", zoom = 6)
g <- ggmap(map)

groups <- c("functional", "functional needs repair", "non functional")
for (i in 1:length(groups)) {
  
  group <- groups[i]
  relevant_rows <- pred_mod_rf2_newdata == group | df_test[, "status_group"] == group
  group_preds <- c()
  for (i in 1:length(pred_mod_rf2_newdata)) {
    if (pred_mod_rf2_newdata[i] == group & df_test[i, "status_group"] == group) {
      group_preds <- c(group_preds, "correct")
    } else if (pred_mod_rf2_newdata[i] != group & df_test[i, "status_group"] == group) {
      group_preds <- c(group_preds, "false negative")
    } else if (pred_mod_rf2_newdata[i] == group & df_test[i, "status_group"] != group) {
      group_preds <- c(group_preds, "false positive")
    } else {
      group_preds <- c(group_preds, "irrelevant")
    }
  }
  
  g + geom_point(data = df_test[relevant_rows,], aes(x = longitude, y = latitude, color = group_preds[relevant_rows]), size = 1)
  fn <- paste0("Accuracy of mod_rf2 for ", group, ".png")
  ggsave(filename=fn, width = 10, height = 10)
  
}


### observations from the map
# 1. many false negatives on 'functional needs repair' - the model does very poorly
# 2. predictions of 'functional' are decent but slightly higher misclassification rate in the SE corner of the country




### ignore below


# # mtry=2, ntree=1000
# # OOB misclassification rate for rf3:  0.1914983
# m <- 2
# n_tree <- 1000
# mod_rf3 <- randomForest(status_group ~ ., data=df_train, mtry=m, ntree=n_tree)
# varImpPlot(mod_rf3)
# 
# # out-of-bag: 23.1% misclassification rate
# pred_mod_rf3_oob <- predict(mod_rf3)  # with newdata unspecified, uses OOB
# misclass_mod_rf3_oob <- sum(pred_mod_rf3_oob != df_train[, "status_group"]) / nrow(df_train)
# cat("OOB misclassification rate for rf3: ", misclass_mod_rf3_oob)
# 
# # new data
# pred_mod_rf3_newdata <- predict(mod_rf3, newdata=df_test)
# misclass_mod_rf3_newdata <- sum(pred_mod_rf3_newdata != df_test[, "status_group"]) / nrow(df_test)
# cat("newdata misclassification rate for rf3: ", misclass_mod_rf3_newdata)




###
### testing to figure out optimum mtry and ntree
### mtry=2, ntree=1000 performed best
###

# ##settings for randomForest
# mtryv = c(2,3)
# ntreev = c(500,1000,1500)
# setrf = expand.grid(mtryv,ntreev)
# colnames(setrf)=c("mtry","ntree")
# 
# ##fit rf
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
#   cat(misclass_tmp, "\n\n")
# }

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

