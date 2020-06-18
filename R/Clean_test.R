#Remove all the objects stored
rm(list=ls())

#Set current working directory
setwd('C:/Users/SAGAR/Downloads/Data Science/Cab_Fare_Prediction')

#Loading important libraries
x = c('ggplot2', 'corrgram', 'DataCombine','lubridate','dplyr','geosphere','gridExtra',
      'DataExplorer','tidyr')

lapply(x, require, character.only = TRUE)


#Read the test data
test = read.csv("test.csv", header = T, na.strings = c(" ", "", "NA"))
summary(test)
dim(test)
str(test)

#Check for Outliers
colSums(is.na(train))


#Extracting Features from pickup datetime variable
test<-test %>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    year = as.factor(year(pickup_datetime)),
    month = as.factor(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayofweek = as.factor(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)),
    timeofday = as.factor(ifelse(hour >= 3 & hour < 9,
                          "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                           ifelse(hour >= 14 & hour < 18, "Evening", "Night")))))



#Removing pickup datetime after extracting info
test=subset(test,select=-pickup_datetime)



#Calculating Haversine distance
test<-test%>%
  mutate(dist = distHaversine(cbind(pickup_longitude, pickup_latitude), 
                              cbind(dropoff_longitude, dropoff_latitude), r = 6371))


#Removing rows with Dist=0
abc=test[which(test$dist==0),] 
test=anti_join(test, abc)


write.csv(test,"test_clean.csv", row.names = F)






