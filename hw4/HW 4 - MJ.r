##Clear everything
rm(list=ls(all=TRUE))

##Don't use scientific notation
options(scipen=5)

###Setup
set.seed(69)
library(dplyr)
library(randomForest)
library(ggplot2)
library(reshape2)
library(gclus)
library(mlbench)
library(caret)
library(gbm)

setwd("/Users/michaeljoyce/hw-machinelearning2k15/hw4/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/hw4") #Jack
#setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/hw4") #Tom


####################################
## Utility functions
####################################
##returns proportion of vector that is na
prop_na <- function(x){return(sum(is.na(x))/length(x))}

##returns proportion of vector with the most common value
prop_most_common_val <- function(x){return(as.numeric(tail(sort(prop.table(table(x))),1)))}

##convert factors with 2 levels to {0,1}
convertFactor <- function(x){
  if(is.factor(x) & nlevels(x) == 2){
    returnVal <- as.numeric(x == levels(x)[2])
  }
  return(returnVal)
}

##scales to 0:1
scf <- function(x) {return((x-min(x))/(max(x)-min(x)))}

##replace NA with mean
replace_na_mean=function(x){
  x[is.na(x)]=mean(x,na.rm=TRUE)
  return(x)
}

#deviance loss function
lossf = function(y,phat,wht=0.0000001) {
  #y should be 0/1
  #wht shrinks probs in phat towards .5, don't log 0!
  if(is.factor(y)) y = as.numeric(y)-1
  phat = (1-wht)*phat + wht*.5
  py = ifelse(y==1,phat,1-phat)
  return(-2*sum(log(py)))
}             

replace_na_impute=function(x, val){
  x[is.na(x)]=val
  return(x)
} 

####################################
## DATA
####################################
raw <- read.csv("data/orange_small_train.data.x_and_y.csv")
features <- select(raw, -(churn:upselling))
Y <- select(raw, (churn:upselling))

###split
train.ind <- sample(seq_len(nrow(features)), size = 4*round(nrow(features)/5,0))
train <- features[train.ind, ]
test <- features[-train.ind, ]

####################################
## CLEANUP: FIND LOW-VARIANCE and HIGH-NA COLUMNS
####################################
drops <- c(1)
for(i in 2:ncol(train)){
  if(prop_na(train[[i]]) > .6) {
    print(paste0("dropping ", names(train)[i], " because 60% of values are NA"))
    drops <- c(drops, i)
    
  } else if(prop_most_common_val(train[[i]]) > .99) {
    print(paste0("dropping ", names(train)[i], " because 99% of values are same"))
    drops <- c(drops, i)
  }
}

####################################
## CLEANUP: DROPPING NA/LV COLS
####################################
train_sans_na <- train[, -drops]
test_sans_na <- test[,-drops]


####################################
## CLEANUP: FIND VALUES TO IMPUTE
####################################
###Store the values we'll impute on the training data so we can do that on the test data too
values_to_impute <- data.frame(var = names(train_sans_na),
                               val = 0)

for(i in 1:ncol(train_sans_na)){
  
  if(is.numeric(train_sans_na[[i]])) {
    train_sans_na[[i]] <- replace_na_mean(train_sans_na[[i]])
    print(paste0("replacing NAs in ", names(train_sans_na)[i], " with mean ", mean(train_sans_na[[i]])))
    values_to_impute[i,2] <- as.character(mean(train_sans_na[[i]]))
    
  } else {
    train_sans_na[[i]] = factor(train_sans_na[[i]], levels=c(levels(train_sans_na[[i]]), "UNKNOWN"))
    train_sans_na[[i]][is.na(train_sans_na[[i]])] = "UNKNOWN"
    values_to_impute[i,2] <- as.character("UNKNOWN")
    
  }
}

####################################
## CLEANUP: USING IMPUTED VALUES
####################################


for (i in 1:ncol(test_sans_na)) {
  if (is.numeric(test_sans_na[[i]])) {
    test_sans_na[[i]] <- replace_na_impute(test_sans_na[[i]],as.numeric(values_to_impute[i,2]))
    print(paste0("replacing NAs in ", names(test_sans_na)[i], " with ", values_to_impute[i,2]))
  } else {
    test_sans_na[[i]] <- replace_na_impute(test_sans_na[[i]],as.factor(values_to_impute[i,2]))
    print(paste0("replacing NAs in ", names(test_sans_na)[i], " with ", values_to_impute[i,2]))
  }
}

####################################
## CLEANUP: DROP ALL CATEGORICAL VARIABLES
####################################
## Vinh says to drop them.
## We should be down to 40 variables now
train_sans_na <- train_sans_na[, sapply(train_sans_na, class) != "factor"]
test_sans_na <- train_sans_na[, sapply(test_sans_na, class) != "factor"]


####################################
## CLEANUP: ADDING BACK Y
####################################
### add Y
### we pick churn
train_sans_na <- cbind(churn = factor(Y[train.ind, "churn"], levels=c(-1, 1), labels=c('no', 'yes')), train_sans_na)
test_sans_na <- cbind(churn = factor(Y[-train.ind, "churn"], levels=c(-1, 1), labels=c('no', 'yes')), test_sans_na)


####################################
## CLEANUP: FIND LINEARLY-PREDICTIVE VARIABLES
####################################
### with categorical variables dropped,
### linear regression is fast on the entire training set of 40,000
lm.coefs <- c()
nbr_features <- ncol(train_sans_na)
for (i in 2:(nbr_features-1)) { # note: 'churn' column is first
  
  n <- 1
  lm.coefs.tmp <- c()
  
  for (j in (i+1):nbr_features) {
    
    mod.lm <- glm(train_sans_na$churn ~ train_sans_na[,i] + train_sans_na[,j], family="binomial")
    
    # save off the p-value
    lm.coefs.tmp[n] = coef(summary(mod.lm))[,4][2]
    
    #cat("trained linear model for column ", i,", observing coef of ", lm.coefs.tmp[n], "\n")
    
  }
  
  # add median 
  lm.coefs[i] = median(lm.coefs.tmp)
  cat("median for run ", i, " is ", lm.coefs[i], "\n")
  
}

# these are the variables with minimum p value across runs
coefs.sig <- lm.coefs < .01
coefs.sig[1] = TRUE


####################################
## CLEANUP: DROP NON-SIGNIFICANT LINEAR PREDICTORS
####################################
# create test/train data sets containing only the significant columns
# rearranging so churn (the 'Y') is first
orange.train <- train_sans_na[, coefs.sig]


# gbm requires Y to be {0,1}
orange.train$churn = convertFactor(orange.train$churn)

# for rf - no factor conversion
orange.train.rf <- train_sans_na[, coefs.sig]

orange.test <- test_sans_na[, coefs.sig]
# gbm requires Y to be {0,1}
orange.test$churn = convertFactor(orange.test$churn)

# for rf - no factor conversion
orange.test.rf <- test_sans_na[, coefs.sig]



####################################
## MODELING: BOOSTING
####################################
# create boosting fit

losstotal=9999999
bestntrees=0
MAX_TREES = 2000
by.step <- 10
totallvec=rep(0,MAX_TREES/by.step)

orange.train$churn=convertFactor(orange.train$churn)
boost.fit = gbm(churn~., 
                distribution = "adaboost", 
                data=orange.train, 
                n.trees=MAX_TREES, 
                interaction.depth = 1,
                shrinkage = 0.01) #haha
#let's figure out the optimal number of trees to boost with
for (ntrees in seq(10, MAX_TREES, by=by.step)) {
  if (ntrees %% 100 == 0) print(paste("Iteration ==>", ntrees))  
  yhat=predict(boost.fit, n.trees=ntrees)
  
  yhatsc=scf(yhat)
  #calculate the total loss of this fit. save off the best loss
  lvec = rep(0,length(yhat))
  for(ii in 1:length(yhat)) lvec[ii] = lossf(orange.train$churn[ii],yhatsc[ii])  
  if (sum(lvec)<losstotal) {
    losstotal=sum(lvec)
    bestntrees=ntrees
    print(paste("bestntrees==>",bestntrees))
  }
  
  #save off the loss for each fit so we can plot it
  totallvec[ntrees/10]=sum(lvec)
}


par(mfrow=c(1,1))
plot(totallvec)

orange.train$churn=convertFactor(orange.train$churn)
# I don't understand the yhat values below... [tom]
best.boost.fit <- gbm(churn~., 
                      distribution = "adaboost", 
                      data=orange.train, 
                      n.trees=bestntrees, 
                      interaction.depth = 1,
                      shrinkage = 0.01) #haha

yhat.best.boost.fit <- predict(best.boost.fit, n.trees=bestntrees, newdata=orange.test)


####################################
## MODELING: RANDOM FOREST
####################################
m <- floor(sqrt(ncol(orange.train.rf)))
fit.rf <- randomForest(churn ~ ., data=orange.train.rf, mtry=m, ntree=500)
pred.oob.rf <- predict(fit.rf) # with newdata unspecified, uses oob

pred.test.rf <- predict(fit.rf, newdata=orange.test.rf)

rf.positive_pred <- pred.test.rf == "yes"
cat("positive predictions: ", sum(rf.positive_pred), "of ", length(pred.test.rf))
pred.test.rf.false_positive <- orange.test.rf[rf.positive_pred, "churn"] == "no"