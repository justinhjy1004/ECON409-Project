setwd("C:/Users/JHo99/Box/Raw Data/Airbnb Detailed Listings")
library(tidyverse)
library(ggplot2)

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

df <- NULL

for(city in citiesList[[1]]){
  nameOfCity <- city
  index.id <- match(nameOfCity,citiesList[[1]])
  cityId <- citiesList[[2]][index.id]
  listingFileExt <- "Listings.csv"
  
  df.temp <- read_csv(str_c(nameOfCity,listingFileExt))
  
  section <- c("id", "host_id", "latitude", "longitude", "price", 
               "weekly_price", "monthly_price", "first_review", "last_review")
  
  df.temp <- df.temp[section]
  df.temp <- df.temp[complete.cases(df.temp[,8:9]),]
  
  df.temp$city <- nameOfCity
  df.temp$cityId <- cityId
  
  df <- rbind(df,df.temp)
}

View(df)

outputPath <- "C:/Users/JHo99/Box/Prepared Data/Airbnb/AirbnbDetailedListings.csv"
write.csv(df, file = outputPath)

temp <- df[df$first_review < dateAvailable[5] & df$last_review > dateAvailable[5],]
length(temp$id)
