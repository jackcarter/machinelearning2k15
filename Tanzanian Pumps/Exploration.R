rm(list = ls())
options(scipen=5)
set.seed(2015)

setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
#setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

library(dplyr)
library(ggplot2)
library(scales)
source("Pump Util.R")
df <- read.csv("Pump_it_Up_Data_Mining_the_Water_Table_-_Training_set_values.csv")

##How much data do we have here?
dim(df)

##30 factor columns, 10 numeric columns
sapply(df, class) %>% as.data.frame() %>% table()

##None of the data is missing!
sapply(df, prop_na) %>% as.data.frame() %>% table()

##What fraction of each column has the column's most common value?
cc <- sapply(df, prop_most_common_val) %>% round(digits = 2) %>% as.data.frame() %>% add_rownames()
qplot(cc$., binwidth = .05)


