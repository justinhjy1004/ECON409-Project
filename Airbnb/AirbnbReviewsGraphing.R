setwd("C:/Users/JHo99/Box/Prepared Data/Airbnb")
library(tidyverse)
library(ggplot2)

df <- read_csv("Airbnb Reviews Count.csv")
cities <- c("SanFrancisco","Nashville","Austin")
df <- df[df$city == cities,]

ggplot(data = df) + 
  geom_line(mapping = aes(x = date, y = count, colour = city))

df1 <- read_csv("Airbnb Listings.csv")

df2 <- read_csv("Airbnb Reviews.csv")

