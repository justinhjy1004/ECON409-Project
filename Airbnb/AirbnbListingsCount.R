setwd("C:/Users/JHo99/Box/Prepared Data/Airbnb")

library(tidyverse)
library(lubridate)

cities <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
            "NewOrleans", "Denver", "Columbus", "Boston", "Austin", "Chicago", 
            "LosAngeles", "NewYork","TwinCities") 

cityIdList <- c(20330, 6118, 13373, 41568, 16037, 19594, 11093, 10920, 44269, 10221,
                17426, 12447, 6181, 20313)

citiesList <- list(cities,cityIdList)


yearAvailable <- 2008 + seq(1:12)
monthAvailable <- seq(1:12)
dateAvailable <- c()
for(y in yearAvailable){
  for(m in monthAvailable){
    date <- str_c(toString(y),"-",toString(m),"-","01")
    dateAvailable <- c(dateAvailable, date)
  }
}

dateAvailable <- dateAvailable[1:135]
dateAvailable <- as.Date(dateAvailable,"%Y-%m-%d")

l <- read_csv("AirbnbDetailedListings.csv")
l <- l[-1]

df <- data.frame("city" = c(1),"cityId" = c(1) ,"date" = c(dateAvailable[1]) ,"listings" = c(1) )

for(city in citiesList[[1]]){
  nameOfCity <- city
  index.id <- match(nameOfCity,citiesList[[1]])
  cityId <- citiesList[[2]][index.id]
  l.city <- l[l$city == nameOfCity,]
  
  for(i in dateAvailable){
    temp <- l.city[l.city$first_review < i & l.city$last_review > i,]
    number <- length(temp$id)
    df <- rbind(df, list(nameOfCity, cityId, as.Date(i, origin="1970-01-01"), number))
  }
}

df <- df[-c(1),]

outputPath <- "C:/Users/JHo99/Box/Prepared Data/Airbnb/AirbnbListingsCount.csv"
write.csv(df, file = outputPath)
