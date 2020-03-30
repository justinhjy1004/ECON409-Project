library(tidyverse)
library(ggplot2)

reviewCounts <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Reviews Count.csv")
listingCounts <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/AirbnbListingsCount.csv")


# plots the number of Reviews in each month based on the city(ies)
graphReviews <- function(cities){
  a <- reviewCounts[reviewCounts$city == cities,]
  
  p <- ggplot(data = a) + 
          geom_line(mapping = aes(x = date, y = count, colour = city)) +
          labs(x = "Date", y = "Number of Airbnb Reviews")
  p
}

# plots the number of unique listings in each month based on the city
graphListings <- function(cities){
  a <- listingCounts[listingCounts$city == cities,]
  a <- a[a$date < as.Date("2019-11-01", origin="1970-01-01"),]
  p <- ggplot(data = a) + 
    geom_line(mapping = aes(x = date, y = listings, colour = city)) +
    labs(x = "Date", y = "Number of Airbnb Listings")
  p
}