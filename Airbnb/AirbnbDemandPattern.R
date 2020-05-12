#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

library(forecast)
library(tsbox)

citiesAirbnb <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
                  "TwinCities", "Denver", "Columbus",
                  "Austin", "Chicago", "LosAngeles", "NewYork")
for(i in 1:length(citiesAirbnb)){
  city <- citiesAirbnb[i]
  review.city <- subsetReviews(city)
  
  start <- min(review.city$date)
  startYear <- as.numeric(substr(start,1,4))
  startMonth <- as.numeric(substr(start,6,7))
  
  
  review.ts <- ts(data = review.city$count, frequency = 12, 
                  start = c(startYear, startMonth))
  
  decomposedReview <- decompose(review.ts, type = "mult")
  print(str_c("City: ", city))
  
  fileName <- str_c("C:/Users/JHo99/Box/Prepared Data/Demand Pattern/",city,".jpg")
  jpeg(fileName)
  plot(decomposedReview) 
  dev.off()
}
  
  seasonalList <- list()
  
  for(i in 1:length(citiesAirbnb)){
    city <- citiesAirbnb[i]
    review.city <- subsetReviews(city)
    review.city <- timeframeListingsAirbnb("2019-12-01", "2015-01-01", review.city)
    
    start <- min(review.city$date)
    startYear <- as.numeric(substr(start,1,4))
    startMonth <- as.numeric(substr(start,6,7))
    
    
    review.ts <- ts(data = review.city$count, frequency = 12, 
                    start = c(startYear, startMonth))
    
    decomposedReview <- decompose(review.ts, type = "mult")
    seasonalList <- list.append(seasonalList, decomposedReview$seasonal)
}


  dateOct <- c(as.POSIXct("2015-10-01"), as.POSIXct("2016-10-01"), as.POSIXct("2017-10-01"),
            as.POSIXct("2018-10-01"), as.POSIXct("2019-10-01"))
  
  dateAug <- c(as.POSIXct("2015-08-01"), as.POSIXct("2016-08-01"), as.POSIXct("2017-08-01"),
               as.POSIXct("2018-08-01"), as.POSIXct("2019-08-01"))
  
  dateMarch <- c(as.POSIXct("2015-03-01"), as.POSIXct("2016-03-01"), as.POSIXct("2017-03-01"),
               as.POSIXct("2018-03-01"), as.POSIXct("2019-03-01"))
  
  dateApr <- c(as.POSIXct("2015-04-01"), as.POSIXct("2016-04-01"), as.POSIXct("2017-04-01"),
                 as.POSIXct("2018-04-01"), as.POSIXct("2019-04-01"))
  
  dev.new(width=5, height=4, unit="in")
  
  ts_plot(
    `Nashville` = seasonalList[[2]],
    `Minneapolis/St.Paul` = seasonalList[[6]],
    `Columbus` = seasonalList[[8]],
    `Denver` = seasonalList[[7]],
    `Austin` = seasonalList[[9]],
    title = "Airbnb Seasonality",
    subtitle = "Smaller cities tend to have more consistent Demand Patterns"
  )
  
  ts_plot(
    `San Francisco` = seasonalList[[1]],
    `Portland` = seasonalList[[3]],
    `Seattle` = seasonalList[[5]],
    `Denver` = seasonalList[[7]],
    `Los Angeles` = seasonalList[[11]],
    `Minneapolis/St.Paul` = seasonalList[[6]],
    title = "Airbnb Seasonality",
    subtitle = "Cities with Peaks in Every August"
  )
  
  abline(v= dateAug, lty=2, col ="red")
  
  ts_plot(
    `Nashville` = seasonalList[[2]],
    `Columbus` = seasonalList[[8]],
    `Chicago` = seasonalList[[10]],
    `New York City` = seasonalList[[12]],
    title = "Airbnb Seasonality",
    subtitle = "Cities with Peaks in Every October"
  )
  
  abline(v= dateOct, lty=2, col ="red")

  ts_plot(
    `Austin` = seasonalList[[9]],
    title = "Airbnb Seasonality of Austin",
    subtitle = "Peaks in March and October"
  )
  abline(v= dateMarch, lty=2, col ="red")
  abline(v= dateOct, lty=2, col ="blue")
 
  ts_plot(
    `Washington DC` = seasonalList[[4]],
    title = "Airbnb Seasonality of Washington DC",
    subtitle = "Peaks in April and October"
  )
  abline(v= dateApr, lty=2, col ="red")
  abline(v= dateOct, lty=2, col ="blue")
  