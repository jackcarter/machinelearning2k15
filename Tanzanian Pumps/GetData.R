rm(list = ls())
options(scipen=5)
set.seed(2015)

library(dplyr)
source("Pump Util.R")

# note: labels list is here:
# http://www.drivendata.org/competitions/7/page/25/#labels_list

features <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_values.csv")
labels <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_labels.csv")
df <- cleanseAndProcessData(merge(labels, features, by = "id"))

# data for submission to DrivenData competition
df_submission <- cleanseAndProcessData(read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Test_set_values.csv"))

# normalize factors in submission data set to match the training set
for (i in 1:ncol(df_submission)) {
  if (class(df_submission[,i]) == "factor") {
    levels(df_submission[,i]) <- levels(df[,(i+1)]) # df has one more column (status_group) than df_submission
  }
}

# 80% test, 20% train
n <- nrow(df)
n1 <- floor(4*n/5)
n2 <- floor(n/5)
ii <- sample(1:n,n)

df_train <- df[ii[1:n1],] 
df_smtrain <- df[ii[1:n2],] 
df_test <- df[ii[(n1+1):n],]

# 
# # prepare data for gbm, which needs the predicted variable to be {0,1}
# 
# is_f <- c()
# is_fnr <- c()
# is_nf <- c()
# 
# for (i in 1:nrow(df)) {
#   sg <- df[i, "status_group"]
#   if (sg == "functional") {
#     is_f[i] = 1
#   } else {
#     is_f[i] = 0  
#   }
#   
#   if (sg == "functional needs repair") {
#     is_fnr[i] = 1
#   } else {
#     is_fnr[i] = 0  
#   }
# 
#   if (sg == "non functional") {
#     is_nf[i] = 1
#   } else {
#     is_nf[i] = 0  
#   }
# 
# }
# 
# df_gbm1 <- cbind(is_f, df[,-c(1,2)]) # remove id and status_group
# df_train_gbm1 <- df_gbm1[ii[1:n1],]
# df_smtrain_gbm1 <- df_gbm1[ii[1:n2],]
# df_test_gbm1 <- df_gbm1[ii[(n1+1):n],]
# 
# df_gbm2 <- cbind(is_fnr, df[,-c(1,2)]) # remove id and status_group
# df_train_gbm2 <- df_gbm2[ii[1:n1],]
# df_smtrain_gbm2 <- df_gbm2[ii[1:n2],]
# df_test_gbm2 <- df_gbm2[ii[(n1+1):n],]
# 
# df_gbm3 <- cbind(is_nf, df[,-c(1,2)]) # remove id and status_group
# df_train_gbm3 <- df_gbm3[ii[1:n1],]
# df_smtrain_gbm3 <- df_gbm3[ii[1:n2],]
# df_test_gbm3 <- df_gbm3[ii[(n1+1):n],]
# 
