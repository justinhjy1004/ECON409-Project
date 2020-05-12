library(tidyverse)

df <- read_csv("C:/Users/JHo99/Box/Raw Data/ZRI/ZRI All Homes.csv")

df$RegionName <- gsub(" ", "", df$RegionName)

cities <- c("Austin", "Boston", "Columbus", "Chicago", "Denver", "LosAngeles",
            "Nashville","NewOrleans", "NewYork", "Portland", "SanFrancisco",
            "Seattle", "SaintPaul", "Minneapolis", "Washington")


cityId <- c(6181, 12447, 17426, 10221, 44269, 10920, 11093, 5983, 19594,
            20313, 16037, 41568, 13373, 6118, 20330)

df$RegionID <- as.numeric(df$RegionID)

df.bind <- NULL
for(i in cityId){
  df.temp <-  df[df$RegionID == i,]
  df.bind <- rbind.data.frame(df.temp, df.bind)
}


df.bind <- gather(df.bind, key = "Year", value = "Rent", `2010-09`:`2019-12`)
df.bind$Year <- paste(df.bind$Year, "-01")
df.bind$Year <- gsub(" ", "", df.bind$Year)


library(lubridate)
df.bind$year <- ymd(df.bind$Year)

nameOfFile <- "ZRI.csv"

fileOutput <- str_c("C:/Users/JHo99/Box/Prepared Data/ZRI Clean/", nameOfFile)
write.csv(df.bind, file = fileOutput)
