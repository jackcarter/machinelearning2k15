###Setup
set.seed(69)
library(dplyr)

###Read in data - drop the labels
features <- read.csv("//Users/michaeljoyce/Downloads/DATA___KDDCup2009_Customer_relationship-master/orange_small_train.data.x_and_y.csv") %>%
  select(-(churn:upselling))

labels <- read.table("//Users/michaeljoyce/Downloads/DATA___KDDCup2009_Customer_relationship-master/orange_small_train_appetency.labels.txt",
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
###Store the values we'll impute on the training data so we can do that on the test data too
values_to_impute <- data.frame(var = names(features_sans_na),
                               val = 0)

for(i in 2:ncol(features_sans_na)){
  
  if(is.numeric(features_sans_na[[i]])) {
    features_sans_na[[i]] <- replace_na_mean(features_sans_na[[i]])
    print(paste0("replacing NAs in ", names(features_sans_na)[i], " with mean"))
    values_to_impute[i,2] <- as.character(mean(features_sans_na[[i]]))
    
  } else {
    features_sans_na[[i]] = factor(features_sans_na[[i]], levels=c(levels(features_sans_na[[i]]), "UNKNOWN"))
    features_sans_na[[i]][is.na(features_sans_na[[i]])] = "UNKNOWN"
    values_to_impute[i,2] <- as.character("UNKNOWN")
    
  }
}

###Outliers? Numeric
###Many values per categorical feature
df <- features_sans_na

factors <- sapply(df, is.factor)
df_factor <- df[,factors]
df_factor_levels <- data.frame(var = names(df_factor),
                               levels = 0)
for (i in 1:ncol(df_factor)){
  df_factor_levels[i,2] <- length(levels(df_factor[,i]))
}

arrange(df_factor_levels, levels)




##RUN SINGLE-VARIABLE CLASSIFIERS?
##AND DROP COLUMNS


##HOW DO WE CLEAN THE TEST DATA?
##WE HAVE TO SAVE THE VALUES TO IMPUTE...
## WE ALSO HAVE TO 
