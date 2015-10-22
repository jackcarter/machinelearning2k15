###Setup
set.seed(69)
library(dplyr)

###Read in data - drop the labels
features <- read.csv("//Users/michaeljoyce/Downloads/DATA___KDDCup2009_Customer_relationship-master/orange_small_train.data.x_and_y.csv") %>%
  select(-(churn:upselling))

labels <- read.table("//Users/michaeljoyce/Downloads/DATA___KDDCup2009_Customer_relationship-master/orange_small_train_appetency.labels.txt",
                     sep = "\t", quote = "", comment = "")



###split
train.ind <- sample(seq_len(nrow(features)), size = round(nrow(features)/2,0))
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
train_sans_na <- train[-drops]


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
##RUN SINGLE-VARIABLE CLASSIFIERS?
##AND DROP COLUMNS
for (i in 1:ncol(test_sans_na)) {
 print(paste0(prop_na(test_sans_na[[i]]),names(test_sans_na)[i]))
}
prop_na(test$Var229)
##HOW DO WE CLEAN THE TEST DATA?
