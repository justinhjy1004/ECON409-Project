source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")

a <- subsetZHVI("Austin", "All Homes")
b <- subsetZHVI("Nashville", "All Homes")
c <- subsetZHVI("Washington", "All Homes")

zhvi.graph <- rbind(a,b,c)

ggplot(data = zhvi.graph) +
  geom_line(mapping = aes(x = Year, y = HousePrice, colour = RegionName)) +
  labs(x = "Date", y = "House Price")

a.1 <- rateOfChangeZHVI(a)
b.1 <- rateOfChangeZHVI(b)
c.1 <- rateOfChangeZHVI(c)

zhvi.graph.1 <- rbind(a.1,b.1,c.1)

ggplot(data = zhvi.graph.1) +
  geom_line(mapping = aes(x = Year, y = RateOfChange, colour = RegionName)) +
  geom_hline(yintercept = 0) +
  labs(x = "Date", y = "Change of House Price")

cities <- c("Austin", "WashingtonDC", "Nashville")

graphListings(cities)

graphReviews(cities)
