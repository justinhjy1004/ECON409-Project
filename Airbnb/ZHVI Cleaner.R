# THIS CLEANER IS APPLICABLE FOR ALL ZILLOW HOME VALUE INDEX FILES
# CHANGE NAMEOFFILE VARIABLE TO CLEAN SPECIFIC FILE

#load tidyverse
library(tidyverse)

#set working directory to files
setwd("C:/Users/JHo99/Downloads/ZHVI")

#switch to the specific file to be cleaned
nameOfFile <- "ZHVI Bottom Tier.csv"

df <- read_csv(nameOfFile)

#camel casing for RegionName
df$RegionName <- gsub(" ", "", df$RegionName)

#cities to be analyzed and the city Id
cities <- c("Austin", "Boston", "Columbus", "Chicago", "Denver", "LosAngeles",
            "Nashville","NewOrleans", "NewYork", "Portland", "SanFrancisco",
            "Seattle", "SaintPaul", "Minneapolis", "Washington")

cityId <- c(6181, 12447, 17426, 10221, 44269, 10920, 11093, 5983, 19594,
            20313, 16037, 41568, 13373, 6118, 20330)

df$RegionID <- as.numeric(df$RegionID)

#select data of interest from csv file
df.bind <- NULL
for(i in cityId){
  df.temp <-  df[df$RegionID == i,]
  df.bind <- rbind.data.frame(df.temp, df.bind)
}

#tidy data 
df.bind <- gather(df.bind, key = "Year", value = "HousePrice", `1996-04`:`2019-12`)
df.bind$year <- paste(df.bind$year, "-01")
df.bind$year <- gsub(" ", "", df.bind$year)

#change Year from char to date
library(lubridate)
df.bind$year <- ymd(df.bind$year)

#indicate type of house according to input file
df.bind$Type <- "Bottom Tier"

#output clean version of file
fileOutput <- str_c("C:/Users/JHo99/Downloads/ZHVI Clean/", nameOfFile)
write.csv(df.bind, file = fileOutput)



