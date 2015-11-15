rm(list = ls())
options(scipen=5)
set.seed(2015)

#setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

library(dplyr)
source("Pump Util.R")

# note: labels list is here:
# http://www.drivendata.org/competitions/7/page/25/#labels_list

features <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_values.csv")
labels <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_labels.csv")
df <- merge(labels, features, by = "id")

#recorded_by is clearly useless - remove it
#summary(df$recorded_by)
df <- df[,-21]

## uncomment to examine column properties
# for (i in 2:ncol(df)) { # skip first row (dependent)
#   col_class <- class(df[,i])
#   if (col_class == "factor") {
#     unique_vals <- length(unique(df[,i]))
#     cat(colnames(df)[i], " is factor with ", unique_vals, " unique values\n")
#     
#   } else{
#     cat(colnames(df)[i], " is ", col_class, "\n")
#   }
# }

# date - transform record date to the number
# of days since the latest record in the data set

df[,"date_recorded"] <- as.Date(df[,"date_recorded"])
latest_date <- df[which.max(df[,"date_recorded"]), "date_recorded"]
# days old
df$record_age <- as.numeric(latest_date - df$date_recorded)
# remove
df <- df[, colnames(df) != "date_recorded"]

# impute means for construction year
avg_year <- mean(df$construction_year[df$construction_year!=0])
df$construction_year[df$construction_year==0] = avg_year

latest_construction_year <- df[which.max(df$construction_year), "construction_year"]

# days since construction
df$construction_age <- latest_construction_year - df$construction_year
# remove
df <- df[, colnames(df) != "construction_year"]

# remove columns with too many categorical values
df <- df[, colnames(df) != "funder"]
df <- df[, colnames(df) != "installer"]
df <- df[, colnames(df) != "wpt_name"]
df <- df[, colnames(df) != "subvillage"]
df <- df[, colnames(df) != "ward"]
df <- df[, colnames(df) != "scheme_name"]
df <- df[, colnames(df) != "lga"]

# remove duplicates - R will code these factors for us
df <- df[, colnames(df) != "region_code"]

# factorize integer codes
df$region_code <- as.factor(df$district_code)



# 80% test, 20% train
n <- nrow(df)
n1 <- floor(4*n/5)
ii <- sample(1:n,n)

df_train <- df[ii[1:n1], -1] #remove id field
df_test <- df[ii[(n1+1):n], -1] # remove id field
