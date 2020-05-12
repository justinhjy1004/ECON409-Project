#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

df <- read_csv("C:/Users/JHo99/Box/Prepared Data/Airbnb/Airbnb Listings.csv")

citiesAirbnb <- c("SanFrancisco", "Nashville", "Portland", "WashingtonDC", "Seattle",
                  "TwinCities", "Denver", "Columbus", "NewOrleans",
                  "Austin", "Chicago", "LosAngeles", "NewYork")

for(i in 1:length(citiesAirbnb)){
  
  city <- citiesAirbnb[i]
  
  df.city <- df[df$city == city,]
  print(str_c("City: ", city))
  print(summary(df.city$price))
  
  q <- quantile(df.city$price, probs = 0.95)
  df.city$include <- df.city$price < q[[1]]
  df.city <- df.city[df.city$include == TRUE,]
  
  p <- ggplot(data = df.city, aes(x = price, colour = room_type)) +
    geom_histogram(binwidth = 20, fill = "white") +
    xlab("Price per day ($)") + 
    ylab("frequency") +
    ggtitle(str_c("Distribution of Airbnb Price in ", city)) +
    theme_classic()
  
  fileName = str_c("C:/Users/JHo99/Box/Prepared Data/Airbnb Price Distribution/",city,".jpg")
  ggsave(filename = fileName, plot = p, device = "jpeg")
  
}


