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
