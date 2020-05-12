library(tidyverse)
library(lubridate)
library(forecast)

cities <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
            "NewOrleans", "Denver", "Columbus", "Boston", "Austin", "Chicago", 
            "LosAngeles", "NewYork","TwinCities") 

cityIdList <- c(20330, 6118, 13373, 41568, 16037, 19594, 11093, 10920, 44269, 10221,
                17426, 12447, 6181, 20313)

citiesList <- list(cities,cityIdList)

reviewCounts <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Reviews Count.csv")
listingCounts <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/AirbnbListingsCount.csv")

#subsetting by cities
subsetReviews <- function(city){
   df <- reviewCounts[reviewCounts$city == city,]
   return(df)
}

subsetListings <- function(city){
  df <- listingCounts[listingCounts$city == city,]
  return(df)
}


# takes in a dataframe/tibble and a date [from] to a date [to], in the format of 
# YYYY-MM-DD in the form of string
# Returns a dataframe according to the month and year provided
timeframeListingsAirbnb <- function(to, from, df){
  
  # Order by year and create vector for all available dates
  df <- df[order(df$date),]
  availableDates <- unique(df$date)
  
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
    df$Section <- df$date == d
    df.temp <- df[df$Section == TRUE,]
    df1 <- rbind(df1,df.temp)
  }
  
  # remove Section column
  df1 <- df1[1:(length(df1)-1)]
  return(df1)
}

rateOfChangeReviews <- function(df){
  # Ensure that one Type and one RegionName
  if(length(df$date) != length(unique(df$date))){
    stop("Duplicate Year variable found! Try subsetting by Type and RegionName
         or remove duplicate value")
  }
  
  # 
  df <- df[order(df$date),]
  df$RateOfChange <- 0
  
  # calculation of rate of change
  for(i in 1:(length(df$date) - 1)){
    if(df$count[i] == 0){
      change <- 0
    } else {
      change <- (df$count[i+1] - df$count[i])/df$count[i]
    }
    df$RateOfChange[i+1] <- change*100
  }
  
  df
}

rateOfChangeListings <- function(df){
  # Ensure that one Type and one RegionName
  if(length(df$date) != length(unique(df$date))){
    stop("Duplicate Year variable found! Try subsetting by Type and RegionName
         or remove duplicate value")
  }
  
  # 
  df <- df[order(df$date),]
  df$RateOfChange <- 0
  
  # calculation of rate of change
  for(i in 1:(length(df$date) - 1)){
    if(df$listings[i] == 0){
      change <- 0
    } else {
      change <- (df$listings[i+1] - df$listings[i])/df$listings[i]
    }
    df$RateOfChange[i+1] <- change*100
  }
  
  df
}

growthAnalysis <- function(city, listing.ts, zri.ts, zhvi.ts){
  zriListing <- data.frame(listingGrowth = listing.ts, RentPriceGrowth = zri.ts)
  tsModelZRI <- tslm(listingGrowth ~ RentPriceGrowth, zriListing)
  
  # linear regression
  zhviListing <- data.frame(listingGrowth = listing.ts, HousePriceGrowth = zhvi.ts)
  tsModelZHVI <- tslm(listingGrowth ~ HousePriceGrowth, zhviListing)
  
  p <- ggplot(data = listing.city) +
    geom_line(mapping = aes(x = date, y = RateOfChange), colour = "red") +
    geom_line(data = zri.city, mapping = aes(x = Year, y = RateOfChange), colour = "blue") +
    geom_line(data = zhvi.city, mapping = aes(x = Year, y = RateOfChange), colour = "black") +
    xlab("Time") + 
    ylab("Rate Of Change") +
    annotate(geom="text", x=as.Date("2017-12-01"), y=20, label="Growth of Listings",
             color="red") +
    annotate(geom="text", x=as.Date("2017-12-01"), y=17, label="Growth of Rent Price",
             color="blue") +
    annotate(geom="text", x=as.Date("2017-12-01"), y=14, label="Growth of House Price",
             color="black")
  
}
