### LABELS ARE ACTUALLY IN THE FEATURES DATASET ALREADY...THEY SHOULD BE DROPPED BUT I HAVEN'T DONE IT YET

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

setwd("~/Documents/R/machinelearning/machinelearning2k15/hw4")


###Read in data - drop the labels
features <- read.csv("data/orange_small_train.data.x_and_y.csv") %>%
  select(-(churn:upselling))

Y <- read.csv("data/orange_small_train.data.x_and_y.csv") %>%
       select((churn:upselling))

labels <- read.table("data/orange_small_train_appetency.labels.txt",
                     sep = "\t", quote = "", comment = "")


###split
train.ind <- sample(seq_len(nrow(features)), size = 4*round(nrow(features)/5,0))
train <- features[train.ind, ]
test <- features[-train.ind, ]
					 
					 
###Figure out what needs cleaning
###Missing values (total)

##RETURNS PROPORTION OF VECTOR THAT IS NA
prop_na <- function(x){return(sum(is.na(x))/length(x))}

##RETURNS PROPORTION OF VECTOR WITH THE MOST COMMON VALUE
prop_most_common_val <- function(x){return(as.numeric(tail(sort(prop.table(table(x))),1)))}



##DROPS COLUMNS
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
train_sans_na <- train[, -drops]


##replace NA with mean
replace_na_mean=function(x){
  x[is.na(x)]=mean(x,na.rm=TRUE)
  return(x)
}

replace_na_mean(c(NA, NA, 1, 2, 3))

###Missing values: numeric
###Missing values: categorical
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

###Outliers? Numeric
###Many values per categorical feature
df <- train_sans_na

factors <- sapply(df, is.factor)
df_factor <- df[,factors]
df_factor_levels <- data.frame(var = names(df_factor),
                               levels = 0)
for (i in 1:ncol(df_factor)){
  df_factor_levels[i,2] <- length(levels(df_factor[,i]))
}

arrange(df_factor_levels, levels)


#####~~~~****TEST DATA PROCESSING BEGIN****~~~~~#####
replace_na_impute=function(x, val){
  x[is.na(x)]=val
  return(x)
} 
test_sans_na <- test[-drops]
for (i in 1:ncol(test_sans_na)) {
  if (is.numeric(test_sans_na[[i]])) {
    test_sans_na[[i]] <- replace_na_impute(test_sans_na[[i]],as.numeric(values_to_impute[i,2]))
    print(paste0("replacing NAs in ", names(test_sans_na)[i], " with ", values_to_impute[i,2]))
  } else {
    test_sans_na[[i]] <- replace_na_impute(test_sans_na[[i]],as.factor(values_to_impute[i,2]))
    print(paste0("replacing NAs in ", names(test_sans_na)[i], " with ", values_to_impute[i,2]))
  }
}



### add Y
### we pick churn
train_sans_na <- cbind(churn = factor(Y[train.ind, "churn"], levels=c(-1, 1), labels=c('no', 'yes')), train_sans_na)
test_sans_na <- cbind(churn = factor(Y[-train.ind, "churn"], levels=c(-1, 1), labels=c('no', 'yes')), test_sans_na)

# take a small sample
orange.tmp <- train_sans_na[1:1000,]



lm.coefs <- c()
for (i in 1:(ncol(orange.tmp)-1)) { # note: 'churn' column is last
  mod.lm <- glm(orange.tmp$churn ~ orange.tmp[,i], family="binomial")

  # save off the p-value
  lm.coefs[i] = coef(summary(mod.lm))[,4][2]
  
  cat("trained linear model for column ", i,"\n")
}

# these are the variables with a p value of 0.1 or greater
coefs.sig <- lm.coefs < .1

# create test/train data sets containing only the significant columns
# rearranging so churn (the 'Y') is first
orange.train <- train_sans_na[, coefs.sig]
orange.train <- cbind(churn = train_sans_na[, "churn"], orange.train)
# gbm requires Y to be {0,1}
orange.train$churn = convertFactor(orange.train$churn)

# for rf - no factor conversion
# HACK - remove categorical variables because it can't handle more than 53
orange.train.rf <- train_sans_na[, coefs.sig]
orange.train.rf <- orange.train.rf[, 1:(ncol(orange.train.rf)-2)] # remove last 2 columns
orange.train.rf <- cbind(churn = train_sans_na[, "churn"], orange.train.rf)

orange.test <- test_sans_na[, coefs.sig]
orange.test <- cbind(churn = test_sans_na[, "churn"], orange.test)
# gbm requires Y to be {0,1}
orange.test$churn = convertFactor(orange.test$churn)

# for rf - no factor conversion
# HACK - remove categorical variables because it can't handle more than 53
orange.test.rf <- test_sans_na[, coefs.sig]
orange.test.rf <- orange.test.rf[, 1:(ncol(orange.test.rf)-2)] # remove last 2 columns
orange.test.rf <- cbind(churn = test_sans_na[, "churn"], orange.test.rf)

# small data for plotting
orange.plottmp <- orange.train[1:10000,]
orange.plottmp.rf <- orange.train.rf[1:10000,]

par(mfrow=c(4,4))
for (i in 2:(ncol(orange.plottmp))) {
  plot(orange.plottmp$churn ~ orange.plottmp[,i], main=names(orange.plottmp)[i], xlab="", ylab="churn", col=c("gray", "red"))
}

# create boosting fit



# convert a factor to {0,1}
convertFactor <- function(x){
  if(is.factor(x) & nlevels(x) == 2){
    returnVal <- as.numeric(x == levels(x)[2])
  }
  
  return(returnVal)
  
}

#scales to 0:1
scf <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}


losstotal=9999999
bestntrees=0
MAX_TREES = 2000
by.step <- 10
totallvec=rep(0,MAX_TREES/by.step)

orange.plottmp$churn=convertFactor(orange.plottmp$churn)
boost.fit = gbm(churn~., 
                distribution = "adaboost", 
                data=orange.plottmp, 
                n.trees=MAX_TREES, 
                interaction.depth = 1,
                shrinkage = 0.01) #haha
#let's figure out the optimal number of trees to boost with
for (ntrees in seq(10, MAX_TREES, by=by.step)) {
  if (ntrees %% 100 == 0) print(paste("Iteration ==>", ntrees))  
  yhat=predict(boost.fit, n.trees=ntrees)

  yhatsc=scf(yhat)
  #deviance loss function
  lossf = function(y,phat,wht=0.0000001) {
    #y should be 0/1
    #wht shrinks probs in phat towards .5, don't log 0!
    if(is.factor(y)) y = as.numeric(y)-1
    phat = (1-wht)*phat + wht*.5
    py = ifelse(y==1,phat,1-phat)
    return(-2*sum(log(py)))
  }             
  
  #calculate the total loss of this fit. save off the best loss
  lvec = rep(0,length(yhat))
  for(ii in 1:length(yhat)) lvec[ii] = lossf(orange.plottmp$churn[ii],yhatsc[ii])  
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


############# random forest

m <- floor(sqrt(ncol(orange.train.rf)))

# NOTE: this takes a LONG time
fit.rf <- randomForest(churn ~ ., data=orange.plottmp.rf, mtry=m, ntree=500)
pred.oob.rf <- predict(fit.rf) # with newdata unspecified, uses oob

# this doesn't work, not sure why
# error: Type of predictors in new data do not match that of the training data.
pred.test.rf <- predict(fit.rf, newdata=orange.test)
