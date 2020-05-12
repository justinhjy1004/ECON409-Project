#Analysis Functions
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/ZHVIAnalysisFunction.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbGraphingFunctions.R")
source("C:/Users/JHo99/Documents/ECON409-Project/Airbnb/AirbnbAnalysisFunction.R")

#Vector containing the cities in ZHVI
citiesZHVI <- c("SanFrancisco", "Nashville", "Portland", "Washington", "Seattle",
                "SaintPaul", "Minneapolis", "Denver", "Columbus",    
                "Boston", "Austin", "Chicago", "LosAngeles", "NewYork")

#significance level
s <- 0.05

#before introduction of Airbnb
from.0 <- "1996-04-01"
to.0 <- "2008-01-01"

#after introduction of Airbnb
from.1 <- "2011-01-01"
to.1 <- "2019-12-01"

print("Report")

for(i in 1:length(citiesZHVI)){
city <- citiesZHVI[i]
for(j in 1:length(types)){
type <- types[j]
df.0 <- rateOfChangeZHVI(timeframeZHVI(from.0, to.0, subsetZHVI(city, type, zhvi)))
df.1 <- rateOfChangeZHVI(timeframeZHVI(from.1, to.1, subsetZHVI(city, type, zhvi)))

mu.0 <- mean(df.0$RateOfChange)
mu.1 <- mean(df.1$RateOfChange)

n.0 <- length(df.0$RateOfChange)
n.1 <- length(df.1$RateOfChange)

sd.0 <- sd(df.0$RateOfChange)
sd.1 <- sd(df.1$RateOfChange)
se.mu <- sqrt(sd.0^2/n.0 + sd.1^2/n.1)

t <- (mu.1 - mu.0 - 0)/ se.mu

degFreedom <- min(n.0,n.1) - 1

pval.mu <-2*(pt(-abs(t),degFreedom))

printOutcome(city, type, from.0, to.0, mu.0, from.1, to.1, mu.1, pval.mu, s)
}
}


      