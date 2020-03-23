setwd("C:/Users/JHo99/Box/Raw Data/Airbnb")

library(tidyverse)

cities <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
            "NewOrleans", "Denver", "Columbus", "Boston", "Austin", "Chicago", 
            "LosAngeles", "NewYork","TwinCities") 

cityId <- c(20330,6118,13373,41568,16037,19594,11093,10920,44269,10221,17426,
            12447,6181,20313)

citiesList <- list(cities,cityId)

df <- NULL

for(city in citiesList[[1]]){
  nameOfCity <- city
  index.id <- match(nameOfCity,citiesList[[1]])
  cityId <- citiesList[[2]][index.id]
  listingFileExt <- "Listings.csv"
  
  df.temp <- read_csv(str_c(nameOfCity,listingFileExt))
  df.temp <- df.temp[c(-16,-5,-4,-2)]
  
  df.temp$city <- nameOfCity
  df.temp$cityId <- cityId
  
  df <- rbind(df,df.temp)
}

outputPath <- "C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Listings.csv"
write.csv(df, file = outputPath)
