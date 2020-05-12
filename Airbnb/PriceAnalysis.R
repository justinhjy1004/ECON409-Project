#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

zhvi.AllHomes <- read_csv("C:/Users/JHo99/Box/Prepared Data/ZHVI Clean/ZHVI All Homes.csv")

# vector containing the cities in ZHVI
citiesZHVI <- c("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
                "SaintPaul", "Minneapolis", "Denver", "Columbus",    
                "Austin", "Chicago", "LosAngeles", "NewYork")

citiesAirbnb <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
                  "TwinCities", "TwinCities","Denver", "Columbus",
                   "Austin", "Chicago", "LosAngeles", "NewYork")


for(i in 1: length(citiesZHVI)){
  city <- citiesZHVI[i]
  cityListing <- citiesAirbnb[i]
  # subset the listing counts
  listing.city <- subsetListings(cityListing)
  listing.city <- timeframeListingsAirbnb("2010-09-01", "2019-12-01", listing.city)
  
  start <- min(listing.city$date)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  listing.ts <- ts(data = listing.city$listings, frequency = 12, 
                   start = c(startYear, startMonth))
  
  # subset cities of ZRI
  zri.city <- subsetZRI(city, zri)
  
  start <- min(zri.city$Year)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  zri.ts <- ts(data = zri.city$Rent, frequency = 12, 
               start = c(startYear, startMonth))
  
  # subset cities of ZHVI
  zhvi.city <- subsetZHVI(city, "All Homes", zhvi)
  zhvi.city <- timeframeZHVI("2010-09-01", "2019-12-01", zhvi.city)
  
  start <- min(zhvi.city$Year)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  zhvi.ts <- ts(data = zhvi.city$HousePrice, frequency = 12, 
                start = c(startYear, startMonth))
  
  zriListing <- data.frame(listing = listing.ts, RentPrice = zri.ts)
  tsModelZRI <- tslm(RentPrice ~ listing, zriListing)
  
  # linear regression
  zhviListing <- data.frame(listing = listing.ts, HousePrice = zhvi.ts)
  tsModelZHVI <- tslm(HousePrice ~ listing, zhviListing)
  
  avg <- 50
  y <- avg + 2.5 * avg
  
  y.rent.0 <- zri.city$Rent[1]
  y.house.0 <- zhvi.city$HousePrice[1]
  
  p <- ggplot(data = listing.city) +
    geom_line(mapping = aes(x = date, y = (listings/(max(listings)))*100 + 100), colour = "red") +
    geom_line(data = zri.city, mapping = aes(x = Year, y = (Rent/(y.rent.0))*100), colour = "blue") +
    geom_line(data = zhvi.city, mapping = aes(x = Year, y = (HousePrice/(y.house.0))*100), colour = "black") +
    xlab("Time") + 
    ylab("Index Value") +
    annotate(geom="text", x=as.Date("2012-12-01"), y= y, label="Index of Listings",
             color="red") +
    annotate(geom="text", x=as.Date("2012-12-01"), y= y - 0.2*avg , label="Index of Rent Price",
             color="blue") +
    annotate(geom="text", x=as.Date("2012-12-01"), y= y - 0.4*avg, label="Index of House Price",
             color="black") +
    ggtitle(city)
  
  fileName <- str_c("C:/Users/JHo99/Box/Prepared Data/Price Plots/",city,".jpg")
  
  print(str_c("City: " ,city))
  print("Time Series Linear Regression between ZRI Growth and Airbnb Listings Growth")
  print(summary(tsModelZRI))
  print("Time Series Linear Regression between ZHVI Growth and Airbnb Listings Growth")
  print(summary(tsModelZHVI))
  print(p)
  
  ggsave(filename = fileName, plot = p, device = "jpeg")
}


df.zhvi <- zhvi.AllHomes 
df.zhvi$date <- df.zhvi$year
df.zhvi$cityId <- df.zhvi$RegionID

listingSub <- listingCounts[listingCounts$listings > 100,]

zhviListing <- merge(df.zhvi, listingSub, by = c("date", "cityId"))

for(city in citiesAirbnb){
  cityLM <- zhviListing[zhviListing$cityId == 13373,]
  lm.airbnb <- lm(cityLM$`House Prices`~cityLM$listings)
  
  ggplot(data = cityLM) +
    geom_point(mapping = aes(x = listings, y = `House Prices`))
}

