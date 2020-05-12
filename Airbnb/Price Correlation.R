#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

zhvi.AllHomes <- read_csv("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean/ZHVI All Homes.csv")

# vector containing the cities in ZHVI
citiesZHVI <- c("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
                "Denver", "Columbus",    
                "Austin", "Chicago", "LosAngeles", "NewYork")

citiesAirbnb <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
                  "TwinCities", "TwinCities","Denver", "Columbus",
                  "Austin", "Chicago", "LosAngeles", "NewYork")


df.zhvi <- zhvi.AllHomes 
df.zhvi$date <- df.zhvi$year
df.zhvi$cityId <- df.zhvi$RegionID

listingSub <- listingCounts[listingCounts$listings > 100,]

zhviListing <- merge(df.zhvi, listingSub, by = c("date", "cityId"))

for(city in citiesZHVI){
  print(city)
  cityLM <- zhviListing[zhviListing$RegionName == city,]
  lm.airbnb <- lm(cityLM$`House Prices`~cityLM$listings)
  summary(lm.airbnb)
  
  fileName <- str_c("C:/Users/JHo99/Box/Prepared Data/Price Correlation/",city,".jpg")
 
  p <- ggplot(data = cityLM, aes(x = listings, y = `House Prices`)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", col = "red") +
    labs(title ="Number of Listings v. House Price", x = "Number of Airbnb Listing", y = "House Price") +
    theme_minimal()
  
  ggsave(filename = fileName, plot = p, device = "jpeg")
}

for(city in citiesZHVI){
  print(city)
  cityLM <- zhviListing[zhviListing$RegionName == city,]
  lm.airbnb <- lm(cityLM$`House Prices`~cityLM$listings)
  print(summary(lm.airbnb))
  
  fileName <- str_c("C:/Users/JHo99/Box/Prepared Data/Price Correlation/",city,".jpg")
  
  p <- ggplot(data = cityLM, aes(x = listings, y = `House Prices`)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", col = "red") +
    labs(title ="Number of Listings v. House Price", x = "Number of Airbnb Listing", y = "House Price") +
    theme_minimal()
  
  ggsave(filename = fileName, plot = p, device = "jpeg")
}

p <- ggplot(data = cityLM, aes(x = listings, y = `House Prices`)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", col = "red") +
  labs(title ="Number of Listings v. House Price", x = "Number of Airbnb Listing", y = "House Price") +
  theme_minimal()

