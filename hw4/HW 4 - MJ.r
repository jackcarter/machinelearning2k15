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

setwd("c:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/hw4")


###Read in data - drop the labels
features <- read.csv("data/orange_small_train.data.x_and_y.csv") %>%
  select(-(churn:upselling))

Y <- read.csv("data/orange_small_train.data.x_and_y.csv") %>%
       select((churn:upselling))

labels <- read.table("data/orange_small_train_appetency.labels.txt",
                     sep = "\t", quote = "", comment = "")



###Figure out what needs cleaning
###Missing values (total)

##RETURNS PROPORTION OF VECTOR THAT IS NA
prop_na <- function(x){return(sum(is.na(x))/length(x))}

##RETURNS PROPORTION OF VECTOR WITH THE MOST COMMON VALUE
prop_most_common_val <- function(x){return(as.numeric(tail(sort(prop.table(table(x))),1)))}

##DROPS COLUMNS
drops <- c()
for(i in 2:ncol(features)){
  if(prop_na(features[[i]]) > .6) {
    print(paste0("dropping ", names(features)[i], " because 60% of values are NA"))
    drops <- c(drops, i)
    
  } else if(prop_most_common_val(features[[i]]) > .99) {
    print(paste0("dropping ", names(features)[i], " because 99% of values are same"))
    drops <- c(drops, i)
  }
}
features_sans_na <- features[-drops]


##replace NA with mean
replace_na_mean=function(x){
  x[is.na(x)]=mean(x,na.rm=TRUE)
  return(x)
}

replace_na_mean(c(NA, NA, 1, 2, 3))

###Missing values: numeric
###Missing values: categorical
for(i in 2:ncol(features_sans_na)){
  
  if(is.numeric(features_sans_na[[i]])) {
    features_sans_na[[i]] <- replace_na_mean(features_sans_na[[i]])
    print(paste0("replacing NAs in ", names(features_sans_na)[i], " with mean"))
  } else {
    features_sans_na[[i]] = factor(features_sans_na[[i]], levels=c(levels(features_sans_na[[i]]), "UNKNOWN"))
    features_sans_na[[i]][is.na(features_sans_na[[i]])] = "UNKNOWN"
  }
}

### add Y
### we pick churn
features_sans_na["churn"] = Y[, "churn"]
features_sans_na["churn"] = as.factor(features_sans_na[, "churn"]) # make factor

# remove index column
features_sans_na <- features_sans_na[, -1]


n <- nrow(features_sans_na)
n2 <- floor(n/5)
n3 <- n-n2
ii <- sample(1:n,n)

# take a small sample
orange.tmp <- features_sans_na[ii[1:1000],]


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
orange.train <- features_sans_na[ii[1:n3], coefs.sig]
orange.train <- cbind(churn = features_sans_na[ii[1:n3], "churn"], orange.train)

orange.test <- features_sans_na[ii[(n3+1):n], coefs.sig]
orange.test <- cbind(churn = features_sans_na[ii[(n3+1):n], "churn"], orange.test)

orange.plottmp <- orange.train[1:1000,]

par(mfrow=c(3,3))
for (i in 2:(ncol(orange.plottmp))) {
  plot(orange.plottmp$churn ~ orange.plottmp[,i], main=names(orange.plottmp)[i], xlab="", ylab="churn")
}




# control <- trainControl(method="repeatedcv", number=10, repeats=0)
# # train the model
# model <- train(churn~., data=orange.plot, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)


#http://www.statmethods.net/graphs/scatterplot.html



