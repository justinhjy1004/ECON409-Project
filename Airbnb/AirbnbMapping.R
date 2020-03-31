library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rjson)
library(tidyverse)
library(ggthemes)
library(maps)

austin <- readLines("C:/Users/JHo99/Box/Raw Data/GeoJSON/AustinGeoJSON.geojson")
sites <- read_csv(url("https://unl.box.com/shared/static/1l1vfvwtgerw4m9nwv7gueurte81rxda.csv"))

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

sites.austin <- sites[sites$city == "Austin",]
date <- as.Date("2016-12-01", origin = "1970-01-01")
point <- sites.austin[sites.austin$first_review < date &
                                     sites.austin$last_review > date,]

sf <- geojson_sf(austin)
geom_

ggplot(data = sf) +
  geom_sf() + 
  geom_point(data = point, aes(x = longitude, y = latitude), size = 1, 
             shape = 21, fill = "red") +
              xlab("Longitude") + ylab("Latitude") +  
  geom_label(aes(x = -97.95, y = 30.5, label = str_c("Austin ", as.character(date))), size = 4, 
             fontface = "bold") + 
  theme_map()

tm_shape(sf) +
  tm_borders() +
  tm_fill()

map.austin <- tm_shape(sf) + tm_polygons() + tm_fill(col = "Land_area")   
