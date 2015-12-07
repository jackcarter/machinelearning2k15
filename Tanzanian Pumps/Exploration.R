#setwd("/Users/michaeljoyce/hw-machinelearning2k15/Tanzanian Pumps/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/Tanzanian Pumps/") #Jack
setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/Tanzanian Pumps/") #Tom

### exploratory data analysis

source("GetData.R")

library(ggplot2)
library(scales)
library(ggmap)
library(reshape2)


##How much data do we have here?
dim(df)

#How many examples do we have of each category?
qplot(df$status_group, xlab="pump classification")

##30 factor columns, 10 numeric columns
#sapply(features, class) %>% as.data.frame() %>% table()

##30 factor columns, 10 numeric columns
sapply(df, class) %>% as.data.frame() %>% table()

##None of the data is missing!
sapply(df, prop_na) %>% as.data.frame() %>% table()

##What fraction of each column has the column's most common value?
cc <- sapply(df, prop_most_common_val) %>% round(digits = 2) %>% as.data.frame() %>% add_rownames()
cc <- select(cc, rowName = rowname, propMostCommon = .) %>% arrange(-propMostCommon)
qplot(cc$propMostCommon, binwidth = .05)

##map fun
map <- get_map("Tanzania", zoom = 6)
g <- ggmap(map)

summary(geo)
g + geom_point(data = df, aes(x = longitude, y = latitude, color = construction_year), size = 0.5)
g + geom_point(data = df, aes(x = longitude, y = latitude, color = status_group), size = 1)
ggsave("Well Status by Location.png", width = 10, height = 10)

g + geom_point(data = df, aes(x = longitude, y = latitude, color = basin), size = 1)
ggsave("Well Status by Basin", width = 10, height = 10)


i = 17
for (i in 3:6) {
  
  x = df[[i]]
  xname = names(df)[i]
  title = paste0("Well Status by ", xname)
  filename = paste0(title, ".png")
  
  if(class(df[[i]]) == "factor"){
    print(paste(i, "factor"))
    
    fh = get_factor_histogram(x,
                             xname,
                             df$status_group,
                             "Well Status",
                             title)
    
  
    fh  
    
    ggsave(filename, plot = fh,
           width = 10, height = 10)
  }
  
  
}

