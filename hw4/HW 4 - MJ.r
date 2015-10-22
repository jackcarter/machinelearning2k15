### LABELS ARE ACTUALLY IN THE FEATURES DATASET ALREADY...THEY SHOULD BE DROPPED BUT I HAVEN'T DONE IT YET

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
for(i in 2:ncol(features_sans_na)){
  
  if(is.numeric(features_sans_na[[i]])) {
    features_sans_na[[i]] <- replace_na_mean(features_sans_na[[i]])
    print(paste0("replacing NAs in ", names(features_sans_na)[i], " with mean"))
  } else {
    features_sans_na[[i]] = factor(features_sans_na[[i]], levels=c(levels(features_sans_na[[i]]), "UNKNOWN"))
    features_sans_na[[i]][is.na(features_sans_na[[i]])] = "UNKNOWN"
  }
}

###Outliers? Numeric
###Many values per categorical feature