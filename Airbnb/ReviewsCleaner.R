setwd("C:/Users/JHo99/Box/Raw Data/Airbnb")

library(tidyverse)
library(lubridate)

cities <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
            "NewOrleans", "Denver", "Columbus", "Boston", "Austin", "Chicago", 
            "LosAngeles", "NewYork","TwinCities") 

cityIdList <- c(20330, 6118, 13373, 41568, 16037, 19594, 11093, 10920, 44269, 10221,
            17426, 12447, 6181, 20313)

df <- NULL

citiesList <- list(cities,cityIdList)

for(city in citiesList[[1]]){
nameOfCity <- city
index.id <- match(nameOfCity,citiesList[[1]])
cityId <- citiesList[[2]][index.id]
listingFileExt <- "Reviews.csv"

df.temp <- read_csv(str_c(nameOfCity,listingFileExt))
df.temp <- df.temp[c(-5,-6)]
df.temp$city <- nameOfCity
df.temp$cityId <- cityId
df.temp$date <- as.Date(df.temp$date)

df <- rbind(df, df.temp)
}

outputPath <- "C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Reviews.csv"
write.csv(df, file = outputPath)


