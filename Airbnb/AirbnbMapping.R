library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rjson)
library(tidyverse)
library(ggthemes)
library(maps)
library(tmap)
library(geojsonio)

cities <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
            "NewOrleans", "Denver", "Columbus", "Boston", "Austin", "Chicago", 
            "LosAngeles", "NewYork","TwinCities")

sites <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/AirbnbDetailedListings.csv")

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

city <- cities[14]

fileInput <- str_c("C:/Users/JHo99/Box/Raw Data/GeoJSON/", city, "GeoJSON.geojson")

cityGeoJSON <- readLines(fileInput)

sf <- geojson_sf(cityGeoJSON)

sites.city <- sites[sites$city == city,]
date <- as.Date("2018-12-01", origin = "1970-01-01")
point <- sites.city[sites.city$first_review < date &
                    sites.city$last_review > date,]

x <- mean(sites[sites$city == city,]$longitude) + 0.6
y <- mean(sites[sites$city == city,]$latitude) + 1.0

ggplot(data = sf) +
  geom_sf() + 
  geom_point(data = point, aes(x = longitude, y = latitude), size = 1, 
             shape = 21, fill = "red") +
  xlab("Longitude") + ylab("Latitude") +  
  geom_label(aes(x = x, y = y, label = str_c(city, " ", substr(date,1,4))), size = 4, 
             fontface = "bold") + 
  theme_map()


for(d in dateAvailable){
  sites.city <- sites[sites$city == city,]
  date <- as.Date(d, origin = "1970-01-01")
  point <- sites.city[sites.city$first_review < date &
                        sites.city$last_review > date,]

filePath <- str_c("C:/Users/JHo99/Box/Prepared Data/Map/",city,"/")
fileName <- str_c(city ,as.character(date),".jpg")

p <- ggplot(data = sf) +
  geom_sf() + 
  geom_point(data = point, aes(x = longitude, y = latitude), size = 1, 
             shape = 21, fill = "red") +
  xlab("Longitude") + ylab("Latitude") +  
  geom_label(aes(x = x, y = y, label = str_c(city, " ", substr(date,1,4))), size = 4, 
             fontface = "bold") + 
  theme_map()

ggsave(filename = str_c(filePath,fileName), plot = p, device = "jpeg")
}

