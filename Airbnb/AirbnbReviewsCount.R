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

reviews <- read_csv("Airbnb Reviews.csv")
reviews <- reviews[-1]

df <- data.frame("city" = c(1),"cityId" = c(1) ,"date" = c(dateAvailable[1]) ,"count" = c(1) )

for(city in citiesList[[1]]){
nameOfCity <- city
index.id <- match(nameOfCity,citiesList[[1]])
cityId <- citiesList[[2]][index.id]

reviews.temp <- reviews[reviews$city == nameOfCity,]
reviews.temp$date <- as.numeric(as.POSIXct(as.Date(reviews.temp$date,"%Y-%m-%d")))

for(i in 1:(length(dateAvailable)-1)){
  from <- as.numeric(as.POSIXct(dateAvailable[i]))
  to <- as.numeric(as.POSIXct(dateAvailable[i+1]))
  timeFrame <- reviews.temp[reviews.temp$date >= from & 
                            reviews.temp$date < to,]
  df <- rbind(df, list(nameOfCity, cityId, as.Date(as.POSIXct(from, origin="1970-01-01")), length(timeFrame$date)))
}
}


df <- df[-c(1),]

View(df)


outputPath <- "C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Reviews Count.csv"
write.csv(df, file = outputPath)
