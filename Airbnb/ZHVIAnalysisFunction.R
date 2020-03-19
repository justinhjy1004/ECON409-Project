setwd("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean")

library(tidyverse)
library(rlist)
library(pipeR)

zhvi <- read_csv("ZHVI Combined.csv")

# city and types in ZHVI Combined.csv
cities <- list("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
            "SaintPaul", "NewOrleans", "Minneapolis", "Denver", "Columbus",    
            "Boston", "Austin", "Chicago", "LosAngeles", "NewYork") 
types <- list("2bedrooms",   "4bedrooms",   "All Homes",   "Bottom Tier", "Condo",      
          "Top Tier" )

# a function that subsets the ZHVI database according to city and type
# returns a dataframe/tibble after subsetting 
subsetZHVI <- function(df, city, type){
  validCity <- list.search(cities, . == city)
  validType <- list.search(types, . == type)
  
  if(length(validCity) != 1){
    stop("Invalid city!")
  }
  if(length(validType) != 1){
    stop("Invalid type!")
  }
  
  df <- df[df$RegionName == city & df$Type == type,]
  
  df
}

# takes in a dataframe/tibble and a date [from] to a date [to], in the format of 
# YYYY-MM-DD
# Returns a dataframe according to the month and year provided
timeframeZHVI <- function(df, from, to){
  
}


# a function that takes in a subset of ZHVI Data and outputs the RateOfChange 
# on a monthly basis
# parameter dataframe/tibble
# return dataframe/tibble
rateOfChangeZHVI <- function(df){
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

