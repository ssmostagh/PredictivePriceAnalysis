setwd("/Users/Susanna/Documents/R")
Data<-read.csv("DataPricingModel.csv")
# Direct price has no change, likely OTA hasn't changed and direct price has changed, no inference
# How often predict wrong price? How often no assumption on direct price?

names(Data) #find the variable names

# Note to self: according to email only consider the direct price and Expedia price, create new data frame
# (simplified data). Didn't include room type due to no room-to-room comparison
SimData <- data.frame(Data$keen.created_at, Data$clientName, Data$hotelName, Data$cheaperDirect, Data$prices.direct, 
                      Data$prices.Expedia, Data$query.currency)
names(SimData)<-sub("Data.","",names(SimData))
SimData$prices.direct <- as.numeric(SimData$prices.direct)
SimData$prices.Expedia <- as.numeric(SimData$prices.Expedia)

# Note: Expedia price "NA" means no avilability for that search criteria on expedia.com
unique(Data$clientName) 
# This has four unique values but note: In email says 5 hotel groups, only four found

# Currency Conversion to standard (USD), conversions for all the different currencies
currencies <- unique(SimData$query.currency)
conversionfactor = c(1.45, 1.14, 0.73, 0.27, 1.03, 1, 2.65, 0.27, 0.27, 0.13, 3.32, 0.015, 0.73, 	0.0146, 0.78, 
                     0.15, 0.34, 0.0017, 0.12, 0.56, 0.68, 0.25, 0.028, 0.12, 0.11, 0.005, 0.27, 0.0092, 0.0081,
                     0.15, 1, 0.021, 0.066, 0.00085, 0.5, 0.18, 0.07, 0.48, 1.41, 0.26)

# creates numeric value for the currency type, currencies[6] = USD = 1
for (i in 1:length(currencies))
{
  SimData$query.currency <- sub(currencies[i], i, SimData$query.currency)
}

SimData$query.currency.conversion <-SimData$query.currency
# For to create the price conversion for each in the data frame
for (i in 1:40)
{
  SimData$query.currency.conversion[SimData$query.currency.conversion == i] <- conversionfactor[i]
}
SimData$query.currency.conversion <- as.numeric(SimData$query.currency.conversion)
SimData$standard.price.direct <- with(SimData, query.currency.conversion*prices.direct)
SimData$standard.price.Expedia <-with(SimData, query.currency.conversion*prices.Expedia)
SimData$standard.price.difference <- SimData$standard.price.direct - SimData$standard.price.Expedia
SimData <- na.omit(SimData) # remove NA values

# Thought process: using solely prices, not room data causes anomalies. Find price differences between direct and
# Expedia. Assumptions that the price is for one room.
SimData$PriceDifference <- SimData$prices.direct - SimData$prices.Expedia
names(SimData)<-sub("keen.created_at","time",names(SimData))

# Analysis - 149 elements in a list, one empty data frame
Data.Hotels<-split(SimData, SimData$hotelName) #create list of data frames to use
Data.Hotels[[5]]<-NULL # removes the null data frame in the list, there are only 148 hotels
No.Inference = 0
Wrong.Prediction = 0

for (i in 1:length(Data.Hotels))
{
  Data.Hotels[[i]][1] <- seq(1,lengths(Data.Hotels[[i]][1]),1)
}
timelengths = rep(0,length(Data.Hotels))
for ( i in 1:length(Data.Hotels)) {timelengths[i] <- lengths(Data.Hotels[[i]][1])}

for (i in 1:length(Data.Hotels))
{
  for (j in 1:(timelengths[i]-1))
  {
    if (Data.Hotels[[i]][j,9] != Data.Hotels[[i]][j+1,9])
    {
      No.Inference = No.Inference + 1
    }
  }
}

for (i in 1:length(Data.Hotels))
{
  for (j in 1:(timelengths[i]-1))
  {
    if (Data.Hotels[[i]][j,9] == Data.Hotels[[i]][j+1,9] && Data.Hotels[[i]][j,10] != Data.Hotels[[i]][j+1,10])
    {
      Wrong.Prediction = Wrong.Prediction + 1
    }
  }
}

# Find anomalies
par(mfrow=c(1,1))
mod <- lm(standard.price.difference ~ standard.price.Expedia+standard.price.direct, data = SimData)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Observations by Cooks Distance")
abline(h=4*mean(cooksd,na.rm=T),col="red")


# Graph shows three significiant anomolies in influential observations. Looking at data for standardized price difference results in
# noting signigicant anomolies being prices over $500 in price difference

SimData.Anoms <- SimData[which(SimData$standard.price.difference < 500), ]
SimData.Anoms <- SimData.Anoms[which(SimData.Anoms$standard.price.difference > -500), ]
 SimData.Anoms <- SimData.Anoms[which(SimData.Anoms$standard.price.direct > 50), ]
# Uncomment the above line in order to run last anomaly simulation/analysis
Data.Hotels.Anoms <- split(SimData.Anoms, SimData.Anoms$hotelName) 
Data.Hotels.Anoms[[5]]<-NULL 
No.Inference.Anoms = 0 
Wrong.Prediction.Anoms= 0

for (i in 1:length(Data.Hotels.Anoms))
{
  Data.Hotels.Anoms[[i]][1] <- seq(1,lengths(Data.Hotels.Anoms[[i]][1]),1)
}
timelengths = rep(0,length(Data.Hotels.Anoms))
for ( i in 1:length(Data.Hotels.Anoms)) {timelengths[i] <- lengths(Data.Hotels.Anoms[[i]][1])}

for (i in 1:length(Data.Hotels.Anoms))
{
  for (j in 1:(timelengths[i]-1))
  {
    if (Data.Hotels.Anoms[[i]][j,9] != Data.Hotels.Anoms[[i]][j+1,9])
    {
      No.Inference.Anoms = No.Inference.Anoms + 1
    }
  }
}

for (i in 1:length(Data.Hotels.Anoms))
{
  for (j in 1:(timelengths[i]-1))
  {
    if (Data.Hotels.Anoms[[i]][j,9] == Data.Hotels.Anoms[[i]][j+1,9] && Data.Hotels.Anoms[[i]][j,10] != Data.Hotels.Anoms[[i]][j+1,10])
    {
      Wrong.Prediction.Anoms = Wrong.Prediction.Anoms + 1
    }
  }
}