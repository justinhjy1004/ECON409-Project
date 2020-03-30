library(tidyverse)
library(rlist)
library(pipeR)

# city and types in ZHVI Combined.csv
cities <- list("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
            "SaintPaul", "NewOrleans", "Minneapolis", "Denver", "Columbus",    
            "Boston", "Austin", "Chicago", "LosAngeles", "NewYork") 
types <- list("2bedrooms",   "4bedrooms",   "All Homes",   "Bottom Tier", "Condo",      
          "Top Tier" )

df <- read_csv("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean/ZHVI.csv")

# a function that subsets the ZHVI database according to city and type
# returns a dataframe/tibble after subsetting 
subsetZHVI <- function(city, type){
  a <- df
  validCity <- list.search(cities, . == city)
  validType <- list.search(types, . == type)
  
  if(length(validCity) != 1){
    stop("Invalid city!")
  }
  if(length(validType) != 1){
    stop("Invalid type!")
  }
  
  a <- a[a$RegionName == city & a$Type == type,]
  
  a
}


# takes in a dataframe/tibble and a date [from] to a date [to], in the format of 
# YYYY-MM-DD in the form of string
# Returns a dataframe according to the month and year provided
timeframeZHVI <- function(from, to){
  df <- read_csv("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean/ZHVI.csv")
  
  # load library lubridate
  library(lubridate)
  
  # Order by year and create vector for all available dates
  df <- df[order(df$Year),]
  availableDates <- unique(df$Year)
 
  # date and month
  startYear <- as.numeric(substr(from,1,4))
  startMonth <- as.numeric(substr(from,6,7))
  endYear <- as.numeric(substr(to,1,4))
  endMonth <- as.numeric(substr(to,6,7))
  
  # determine the start date and month
  startDate <- ymd(str_c(startYear, startMonth, "01", sep = "-"))
  endDate <- ymd(str_c(endYear, endMonth, "01", sep = "-"))
  startIndex <- match(startDate, availableDates)
  endIndex <- match(endDate, availableDates)
  
  # error handling for date to check if it is valid
  if(is.na(startIndex)){
    stop("Start date is invalid!")
  }
  if(is.na(endIndex)){
    stop("End date is invalid!")
  }
  
  # subset dates to be taken
  dateFrame <- availableDates[startIndex:endIndex]
  
  # iterate dataframe to subset dataframe
  df1 <- data.frame()
  for(d in dateFrame){
    df$Section <- df$Year == d
    df.temp <- df[df$Section == TRUE,]
    df1 <- rbind(df1,df.temp)
  }
  
  # remove Section column
  df1 <- df1[1:(length(df1)-1)]
  df1
}


# a function that takes in a subset of ZHVI Data and outputs the RateOfChange 
# on a monthly basis
# parameter dataframe/tibble
# return dataframe/tibble
rateOfChangeZHVI <- function(df){
    # Ensure that one Type and one RegionName
  if(length(df$Year) != length(unique(df$Year))){
    stop("Duplicate Year variable found! Try subsetting by Type and RegionName
         or remove duplicate value")
  }
  
  # 
  df <- df[order(df$Year),]
  df$RateOfChange <- 0
  
  # calculation of rate of change
  for(i in 1:(length(df$Year) - 1)){
    change <- (df$HousePrice[i+1] - df$HousePrice[i])/df$HousePrice[i]
    df$RateOfChange[i+1] <- change*100
  }
  
  df
}
