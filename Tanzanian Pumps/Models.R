#### predictive modeling

source("GetData.R")

library(randomForest)

m <- round(sqrt(ncol(df_train)))
n_tree <- 500

#mod_rf <- randomForest(status_group ~ ., data=df_train, mtry=m, ntree=n_tree)
#pred.oob.rf <- predict(fit.rf) # with newdata unspecified, uses oob
#pred.test.rf <- predict(fit.rf, newdata=uc.test)
