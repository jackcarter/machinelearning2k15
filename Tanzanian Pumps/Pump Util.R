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

##returns a histogram of factor levels vs outcomes
get_factor_histogram <- function(x, xname, y, yname, title){
  
  print(paste('plotting',title))
  
  ##Reorder the factor levels of x by decreasing frequency
  xt = table(x)
  x <- factor(x, levels = names(xt[order(xt, decreasing = TRUE)]))
  
  t = table(x, y) %>% as.data.frame()
  histo = ggplot(data = t, aes(x = x, y = Freq)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~y) +
    coord_flip() + 
    ggtitle(title) +
    theme_bw()
  
  return(histo)
}


##data cleansing
cleanseAndProcessData <- function(df){
  
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
  
  
  ### lame code follows to reduce the dimensionality of 'lga' to the top 29 
  ### regions, plus "other" for less common regions
  
  # # explore values of 'lga' -- this may be valuable if dimensionality is reduced
  # lga_summary <- df %>% 
  #   group_by(lga) %>%
  #   summarise(lga_observations = length(lga)) %>%
  #   arrange(desc(lga_observations))
  # 
  # #View(lga_summary)
  # 
  # min_lga_cnt <- lga_summary$lga_observations[29] # take top 29, and 30 and below will be "other"
  # df <- merge(df, lga_summary, by = "lga")
  # 
  # for (i in 1:nrow(df)) {
  #   if (df[i,"lga_observations"] >= min_lga_cnt) {
  #     df[i, "lga_grp"] <- as.character(df[i, "lga"])
  #   } else {
  #     df[i, "lga_grp"] <- as.factor("OTHER")
  #   }
  # }
  # 
  # # factorize
  # df$lga_grp <- as.factor(df$lga_grp)
  # # remove
  # df <- df[, colnames(df) != "lga_observations"]
  
  
  # remove
  df <- df[, colnames(df) != "construction_date"]
  
  # remove string dates
  df <- df[, colnames(df) != "construction_year"]
  df <- df[, colnames(df) != "date_recorded"]
  
  #recorded_by is clearly useless - remove it
  df <- df[, colnames(df) != "recorded_by"]
  
  # recode for common "funder"
  c <- count(features, "funder")
  c <- c[order(-c$freq),]
  topfunders <- c[1:52,"funder"]
  
  
  ### annoying crap to re-code "funder" as the top funders, plus a 
  ### catch-all "otherfunder" category
  
  # add level
  df$funder <- factor(df$funder, levels=c(levels(df$funder), "otherfunder"))
  
  for (i in 1:nrow(df)) {
    funder <- df[i, "funder"]
    if (funder == "" | funder == "0" | is.na(funder) | !(funder %in% topfunders)) {
      df[i, "gfunder"] <- "otherfunder"
    } else {
      df[i, "gfunder"] <- funder
    }
  }
  
  # fix levels
  df$gfunder <- factor(df$gfunder)

  
#   # recode for common "installer"
#   c <- count(features, "installer")
#   c <- c[order(-c$freq),]
#   top50installers <- c[1:50,"installer"]
#   
#   ### annoying crap to re-code "installer" as the top installers, plus a 
#   ### catch-all "otherinstaller" category
#   
#   # add level
#   df$installer <- factor(df$installer, levels=c(levels(df$installer), "otherinstaller"))
#   
#   for (i in 1:nrow(df)) {
#     installer <- df[i, "installer"]
#     if (installer == "" | installer == "0" | is.na(installer) | !(installer %in% top50installers)) {
#       df[i, "ginstaller"] <- "otherinstaller"
#     } else {
#       df[i, "ginstaller"] <- installer
#     }
#   }
#   
#   # fix levels
#   df$ginstaller <- factor(df$ginstaller)
  
  
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
  
  
  # low value
  df <- df[, colnames(df) != "amount_tsh"]
  df <- df[, colnames(df) != "basin"]
  df <- df[, colnames(df) != "management"]
  df <- df[, colnames(df) != "scheme_management"]
  df <- df[, colnames(df) != "water_quality"]
  
  # remove id field
  #df <- df[, colnames(df) != "id"]
  
  #Impute population from nearest neighbors
  library(DMwR)
  impute.pop <- df[, colnames(df) %in% c("latitude","longitude","population")]
  #Populations of 0 or 1 are probably missing data
  impute.pop$population[impute.pop$population == 0] <- NA
  impute.pop$population[impute.pop$population == 1] <- NA
  impute.pop <- knnImputation(impute.pop)
  df$population=impute.pop$population
  
  return(df)
}
