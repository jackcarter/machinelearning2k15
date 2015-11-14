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