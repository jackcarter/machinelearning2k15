rm(list = ls())
options(scipen=5)
set.seed(2015)

library(dplyr)
source("Pump Util.R")

# note: labels list is here:
# http://www.drivendata.org/competitions/7/page/25/#labels_list

features <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_values.csv")
labels <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_labels.csv")
df <- merge(labels, features, by = "id")

# # uncomment to examine column properties
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

# days between construction and recording
# for missing data, leave at 0
known_construction_year <- df[,"construction_year"] != 0
unknown_construction_year <- df[,"construction_year"] == 0

df[known_construction_year, "construction_date"] <- as.Date(paste0(floor(df[known_construction_year, "construction_year"]),"-12-31")) # last day of year

df[known_construction_year, "days_elapsed"] <- as.numeric(df[known_construction_year, "date_recorded"] - df[known_construction_year, "construction_date"])
df[unknown_construction_year, "days_elapsed"] <- mean(df[known_construction_year, "days_elapsed"]) #impute

# impute means for construction year
avg_year <- mean(df$construction_year[df$construction_year!=0])
df$construction_year[df$construction_year==0] <- avg_year

latest_construction_year <- df[which.max(df$construction_year), "construction_year"]

# days since construction
df$construction_age <- latest_construction_year - df$construction_year

# remove
df <- df[, colnames(df) != "construction_date"]

# remove string dates
df <- df[, colnames(df) != "construction_year"]
df <- df[, colnames(df) != "date_recorded"]

#recorded_by is clearly useless - remove it
df <- df[, colnames(df) != "recorded_by"]

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

# not much new information in these extra extraction_type columns
df <- df[, colnames(df) != "extraction_type"]
df <- df[, colnames(df) != "extraction_type_class"]

# duplicate
df <- df[, colnames(df) != "quantity_group"]
df <- df[, colnames(df) != "source_type"]
df <- df[, colnames(df) != "source_class"]
df <- df[, colnames(df) != "waterpoint_type_group"]
df <- df[, colnames(df) != "payment_type"]
df <- df[, colnames(df) != "management_group"]
df <- df[, colnames(df) != "quality_group"]

# seem to have low signal
df <- df[, colnames(df) != "permit"]
df <- df[, colnames(df) != "public_meeting"]

# junk
df <- df[, colnames(df) != "num_private"]

# factorize integer codes
df$district_code <- as.factor(df$district_code)

### impute means for gps_height
mean_gps_height <- mean(df$gps_height, na.rm=TRUE)
df[df$gps_height == 0, "gps_height"] <- mean_gps_height


# 80% test, 20% train
n <- nrow(df)
n1 <- floor(4*n/5)
n2 <- floor(n/5)
ii <- sample(1:n,n)

df_train <- df[ii[1:n1], -1] #remove id field
df_smtrain <- df[ii[1:n2], -1] #remove id field
df_test <- df[ii[(n1+1):n], -1] # remove id field
