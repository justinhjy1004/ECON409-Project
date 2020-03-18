setwd("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean")

# a function that takes in a subset of 
rateOfChange <- function(df){
  if(length(df$Year) != length(unique(df$Year))){
    stop("Duplicate Year variable found! Try subsetting by Type and RegionName
         or remove duplicate value")
  }
  df <- df[order(df$Year),]
  df$RateOfChange <- 0
  
  for(i in 1:(length(df$Year) - 1)){
    change <- (df$HousePrice[i+1] - df$HousePrice[i])/df$HousePrice[i]
    df$RateOfChange[i+1] <- change*100
  }
  
  df
}

