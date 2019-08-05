library(dplyr)
house_price <- read.csv("House_Price.csv")
house_price[house_price == ""] = NA 
house_sample <- sample_frac(house_price,0.10)


cln <- dimnames(house_sample)
cln_1 <- as.vector(cln[2])
cln_1 <- unlist(cln_1)

for(i in cln_1){
  feature <- i
  if(!is.factor(house_sample[,feature])){
    
    replace <- mean(house_sample[,feature],na.rm = TRUE)
    house_sample[,feature] = ifelse(is.na(house_sample[,feature]), replace, house_sample[,feature])
    
  } else {
    house_sample[,feature] = ifelse(is.na(house_sample[,feature]), "Y", house_sample[,feature])
    # > anyNA(house_sample$WIFI)    | Before this iteration
    # [1] TRUE
    house_sample[,feature] = ifelse(house_sample[,feature] =="Y", 3, house_sample[,feature])
    # > anyNA(house_sample$WIFI)    | After the iteration
    # [1] FALSE
  }
}

