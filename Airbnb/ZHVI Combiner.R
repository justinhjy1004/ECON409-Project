#load tidyverse library
library(tidyverse)

#set working directory for data
#setwd("C:/Users/JHo99/Downloads/ZHVI Clean")

#load dataframes of all different ZHVI metrics
#remove first column that is added by R
df.2 <- read_csv("ZHVI 2bedrooms.csv")
df.2 <- df.2[-1]
df.4 <- read_csv("ZHVI 4bedrooms.csv")
df.4 <- df.4[-1]
df.all <- read_csv("ZHVI All Homes.csv")
df.all <- df.all[-1]
df.bot <- read_csv("ZHVI Bottom Tier.csv")
df.bot <- df.bot[-1]
df.con <- read_csv("ZHVI Condo.csv")
df.con <- df.con[-1]
df.top <- read_csv("ZHVI Top Tier.csv")
df.top <- df.top[-1]

#vertical join tables
df.comb <- rbind(df.2, df.4, df.all, df.bot, df.con, df.top)

#standardizing camel casing
df.comb$Year <- df.comb$year
df.comb$HousePrice <- df.comb$`House Prices`
df.comb$Type <- df.comb$type
df <- subset(df.comb, select = c(1:6, 10:12))

#select output path and write csv
outputPath <- "C:/Users/JHo99/Box/Prepared Data/ZHVI Clean/ZHVI Combined.csv"
write.csv(df, file = outputPath)
