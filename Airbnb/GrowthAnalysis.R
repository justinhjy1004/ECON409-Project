#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

library(ggrepel)

# vector containing the cities in ZHVI
citiesZHVI <- c("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
                "SaintPaul", "Minneapolis", "Denver", "Columbus",    
                "Austin", "Chicago", "LosAngeles", "NewYork")

citiesAirbnb <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
                  "TwinCities", "TwinCities", "Denver", "Columbus"
                  , "Austin", "Chicago", "LosAngeles", "NewYork")


#for(i in 1: length(citiesZHVI)){
  i <- 1
  city <- citiesZHVI[i]
  cityListing <- citiesAirbnb[i]
  # subset the listing counts
  listing.city <- rateOfChangeListings(subsetListings(cityListing))
  listing.city <- timeframeListingsAirbnb("2014-09-01", "2019-06-01", listing.city)
  
  start <- min(listing.city$date)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  listing.ts <- ts(data = listing.city$RateOfChange, frequency = 12, 
                   start = c(startYear, startMonth))
  
  # subset cities of ZRI
  zri.city <- rateOfChangeZRI(subsetZRI(city, zri))
  zri.city <- timeframeZHVI("2014-09-01","2019-06-01",zri.city)
  
  start <- "2014-09-01"
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  zri.ts <- ts(data = zri.city$RateOfChange, frequency = 12, 
               start = c(startYear, startMonth))
  
  # subset cities of ZHVI
  zhvi.city <- rateOfChangeZHVI(subsetZHVI(city, "All Homes", zhvi))
  zhvi.city <- timeframeZHVI("2014-09-01", "2019-06-01", zhvi.city)
  
  start <- min(zhvi.city$Year)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  zhvi.ts <- ts(data = zhvi.city$RateOfChange, frequency = 12, 
                start = c(startYear, startMonth))
  
  zriListing <- data.frame(listingGrowth = listing.ts, RentPriceGrowth = zri.ts)
  tsModelZRI <- tslm(RentPriceGrowth ~ listingGrowth, zriListing)
  
  # linear regression
  zhviListing <- data.frame(listingGrowth = listing.ts, HousePriceGrowth = zhvi.ts)
  tsModelZHVI <- tslm(HousePriceGrowth ~ listingGrowth, zhviListing)
  
  avg <- mean(listing.city$RateOfChange)
  
  p <- ggplot(data = listing.city) +
    geom_line(mapping = aes(x = date, y = RateOfChange), colour = "#31ba1c") +
    geom_text_repel(data = subset(listing.city, date == as.Date("2016-06-01")),
                    aes(x = date, y = RateOfChange, label = "Growth of Listings"),nudge_y = 0.5 *avg, col = "#31ba1c") +
    geom_line(data = zri.city, mapping = aes(x = Year, y = RateOfChange), colour = "#3f55e8") +
    geom_text_repel(data = subset(zri.city, Year == as.Date("2017-06-01")),
                    aes(x = Year, y = RateOfChange + 0.5, label = "Growth of House Price"), nudge_y = (1.5 * avg),col =  "#3f55e8") +
    geom_line(data = zhvi.city, mapping = aes(x = Year, y = RateOfChange), colour = "#ff2014") +
    geom_text_repel(data = subset(zhvi.city, Year == as.Date("2019-01-01")),
                    aes(x = Year, y = RateOfChange - 0.5, label = "Growth of Rent"), nudge_y = (-0.5 * avg), col = "#ff2014") +
    xlab("Time") + 
    ylab("Rate of Change") +
    ggtitle("Rate of Change of Airbnb Listings, House Prices and Rent Prices") +
    theme_minimal()
  
  
  fileName <- str_c("C:/Users/JHo99/Box/Prepared Data/Growth Plots/",city,".jpg")
  
  print(str_c("City: " ,city))
  print("Time Series Linear Regression between ZRI Growth and Airbnb Listings Growth")
  print(summary(tsModelZRI))
  print("Time Series Linear Regression between ZHVI Growth and Airbnb Listings Growth")
  print(summary(tsModelZHVI))
  print(p)
  
  ggsave(filename = fileName, plot = p, device = "jpeg")
#}
