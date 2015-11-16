#setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

#### Random Forests

source("GetData.R")

library(gbm)


##settings for boosting
idv = c(6, 4)
ntv = c(8000, 4000)
shv = c(.1)
setboost = expand.grid(idv,ntv,shv)
colnames(setboost) = c("tdepth","ntree","shrink")

##fit boosting
for(i in 1:nrow(setboost)) {
  cat("on boosting fit ",i,"\n")
  print(setboost[i,])
  
    ##fit and predict
  fboost1 <- gbm(is_f ~ .,
                 data=df_train_gbm1,
                 distribution="bernoulli",
                 interaction.depth=setboost[i,1],
                 n.trees=setboost[i,2],
                 shrinkage=setboost[i,3])
  pred_is_f <- predict(fboost, newdata=df_test_gbm1, n.trees=setboost[i,2], type="response")
  
  ##fit and predict
  fboost2 <- gbm(is_fnr ~ .,
                 data=df_train_gbm2,
                 distribution="bernoulli",
                 interaction.depth=setboost[i,1],
                 n.trees=setboost[i,2],
                 shrinkage=setboost[i,3])
  pred_is_fnr <- predict(fboost2, newdata=df_test_gbm2, n.trees=setboost[i,2], type="response")
  
  ##fit and predict
  fboost3 <- gbm(is_nf ~ .,
                 data=df_train_gbm3,
                 distribution="bernoulli",
                 interaction.depth=setboost[i,1],
                 n.trees=setboost[i,2],
                 shrinkage=setboost[i,3])
  pred_is_nf <- predict(fboost3, newdata=df_test_gbm3, n.trees=setboost[i,2], type="response")
  
  gbm_pred <- c()
  for (i in 1:length(pred_is_f)) {
    f <- pred_is_f[i]
    fnr <- pred_is_fnr[i]
    nf <- pred_is_nf[i]
    
    scores <- c(f, fnr, nf)
    max_score <- max(scores)
    winner <- ""
    
    if (max_score == f) {
      winner <- "functional"
    } else if (max_score == fnr) {
      winner <- "functional needs repair"
    } else if (max_score == nf) {
      winner <- "non functional"
    } else {
      winner <- "uhhhh" 
    }
    
    gbm_pred <- c(gbm_pred, winner)
  }
  
  misclass_mod_gbm <- sum(gbm_pred != df_test[, "status_group"]) / nrow(df_test)
  cat("newdata misclassification rate for gbm: ", misclass_mod_gbm, "\n")
}










########### ignore below

# 
# 
# ##fit and predict
# fboost1 <- gbm(is_f ~ .,
#                data=df_smtrain_gbm1,
#                distribution="bernoulli",
#                n.trees=1000,
#                interaction.depth=4,
#                shrinkage=.01)
# pred_is_f <- predict(fboost, newdata=df_test_gbm1, n.trees=1000, type="response")
# 
# ##fit and predict
# fboost2 <- gbm(is_fnr ~ .,
#                data=df_smtrain_gbm2,
#                distribution="bernoulli",
#                n.trees=1000,
#                interaction.depth=4,
#                shrinkage=.01)
# pred_is_fnr <- predict(fboost2, newdata=df_test_gbm2, n.trees=1000, type="response")
# 
# ##fit and predict
# fboost3 <- gbm(is_nf ~ .,
#                data=df_smtrain_gbm3,
#                distribution="bernoulli",
#                n.trees=1000,
#                interaction.depth=4,
#                shrinkage=.01)
# pred_is_nf <- predict(fboost3, newdata=df_test_gbm3, n.trees=1000, type="response")
# 
# gbm_pred <- c()
# for (i in 1:length(pred_is_f)) {
#   f <- pred_is_f[i]
#   fnr <- pred_is_fnr[i]
#   nf <- pred_is_nf[i]
#   
#   scores <- c(f, fnr, nf)
#   max_score <- max(scores)
#   winner <- ""
#   
#   if (max_score == f) {
#     winner <- "functional"
#   } else if (max_score == fnr) {
#     winner <- "functional needs repair"
#   } else if (max_score == nf) {
#     winner <- "non functional"
#   } else {
#     winner <- "uhhhh" 
#   }
#   
#   gbm_pred <- c(gbm_pred, winner)
# }
# 
# 
# misclass_mod_gbm <- sum(gbm_pred != df_test[, "status_group"]) / nrow(df_test)
# cat("newdata misclassification rate for gbm: ", misclass_mod_gbm)