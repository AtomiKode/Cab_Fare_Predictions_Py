#Remove all the objects stored
rm(list=ls())

#Set current working directory
setwd('C:/Users/SAGAR/Downloads/Data Science/Cab_Fare_Prediction')

#Loading important libraries
x = c('ggplot2', 'corrgram', 'DataCombine','lubridate','dplyr','geosphere','gridExtra',
      'DataExplorer','tidyr')

lapply(x, require, character.only = TRUE)


#Read the train data
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
summary(train)
dim(train)
str(train)


#Cheking the missing values
colSums(is.na(train))


#Creating a copy of variable
df=train

##################################### Outlier values for Passenger count #######################################

abc=df[which(df$passenger_count<1 & df$passenger_count>0),]
df=df[!(df$passenger_count %in% abc$passenger_count),]

abc=df[which(df$passenger_count>6),]
df=df[!(df$passenger_count %in% abc$passenger_count),]

#Note: Here, we are keeping the 0 values because these are actually the missing values which
#      we will later convert into NA so that we can impute them.

####################################### Outlier values for fare amount ##########################################

abc=df[which(df$fare_amount<1 | df$fare_amount>150),]
df=df[!(df$fare_amount %in% abc$fare_amount),]

###################################### Outlier value for GPS Co-Ordinates #######################################

## For Pickup Longitude and Latitude

df[df == 0] <- NA
summary(df)

abc=df[which(df$pickup_latitude<40 | df$pickup_latitude>44),]
df=df[!(df$pickup_latitude %in% abc$pickup_latitude),]

abc=df[which(df$pickup_longitude>= -71 | df$pickup_longitude<= -76),]

summary(df)

## For Dropoff Longitude and Latitude

abc=df[which(df$dropoff_latitude<40 | df$dropoff_latitude>44),]
df=df[!(df$dropoff_latitude %in% abc$dropoff_latitude),]

abc=df[which(df$dropoff_longitude>= -71 | df$dropoff_longitude<=-75),]
df=df[!(df$dropoff_longitude %in% abc$dropoff_longitude),]

summary(df)

######################################### Missing Value Imputation #############################################
 
library(DMwR)

df2=df

#reset index
rownames(df2) <- NULL  



# KNN Imputation Results
#Original :  -73.98713        40.73314         -73.99157         40.75809
#k= 5     :  -73.98548        40.75139         -73.98485         40.76143
#k=10     :  -73.98539        40.74957         -73.98822         40.75673 <-
#k=15     :  -73.98341        40.75476         -73.98517         40.75774


#Values are close to original for k=10

df2[4,3:6] <- NA
df2 = knnImputation(df2, k = 10)

summary(df2)


#Convert passenger count to int
df2$passenger_count = as.integer(round(df2$passenger_count))



#Extracting Features from pickup datetime variable
df2<-df2%>%
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


#Removing pickup_datetime after extraction of info is done
df2=subset(df2,select=-pickup_datetime)



#Creating copy of the variable
df=df2

##Calculating Haversine Distance

df<-df2%>%
  mutate(dist = distHaversine(cbind(pickup_longitude, pickup_latitude), 
                              cbind(dropoff_longitude, dropoff_latitude), r = 6371))




#Deleting values for which distance is 0 and fare amount is $2.5 => This indicates that the customer
#cancelled the cab just after booking it, and since $2.5 is the base amount, the customer had to pay it.
#These values have no use in our analysis.

abc=df[which(df$dist==0 & df$fare_amount==2.5 ),]
df=anti_join(df, abc)


#Some observations have distance as 0 but fare amuont is more than 2.5. This is because pickup 
#and dropoff coordinates for these observations are same which indicates a round trip.
  #distance = (fare_amount - 2.5)/1.56
df3=df[which(df$dist==0),]
df=anti_join(df, df3)

df3$dist = with(df3,((fare_amount - 2.5)/1.56) )

df = rbind(df,df3)
summary(df)



#Some values seem to have very less distance travelled and the fare is very high. These erroneous values are
#to be deleted.
df3=df[which(df$dist<0.1& df$fare_amount>2.5),]
df=anti_join(df, df3)

write.csv(df2, "train_clean.csv", row.names = F)

summary(df) 

######################################### Exploratory data analysis ############################################

library("scales")
library("gplots")


#Scatter plot of Distance vs fareamount
ggplot(df, aes_string(x = df$dist, y = df$fare_amount)) + 
  geom_point(aes_string(colour =  df$fare_amount),size = 4) +
  theme_bw()+ ylab("fare amount") + xlab("distance") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=25)) 


#Removing some outlier values
df3=df[which(df$dist>75 | df$fare_amount>100),]
df=anti_join(df, df3)

#Scatter plot of Distance vs fareamount
ggplot(df, aes_string(x = df$dist, y = df$fare_amount)) + 
  geom_point(aes_string(colour =  df$fare_amount),size = 4) +
  theme_bw()+ ylab("fare amount") + xlab("distance") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=25)) 


#Finally writing the clean data to csv
write.csv(df, "train_clean.csv", row.names = F)


#Plotting Correlation plot
library(corrplot)
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)

corr=cor(numeric_data)
corrplot(corr, method="circle" )

summary(df)

##################### Passenger Count #################################

ggplot(df, aes_string(x = df$passenger_count)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6))+
  theme_bw() + xlab("Passenger Count") + ylab("Frequency") + ggtitle("Passenger Count Distribution") +
  theme(text=element_text(size=20))


#Multivariate #Scatter Plot
ggplot(df, aes_string(x = df$passenger_count, y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Passenger count") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6))
  

##################### Year #######################

ggplot(df, aes_string(x = df$year)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6))+
  theme_bw() + xlab("Year") + ylab("Frequency") + ggtitle("Yearwise Distribution") +
  theme(text=element_text(size=20))


#Multivariate #Scatter Plot
ggplot(df, aes_string(x = df$year, y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Year") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=20))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=6))

################## Month ########################


ggplot(df, aes_string(x = df$month)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=12))+
  theme_bw() + xlab("Month") + ylab("Frequency") + ggtitle("Monthwise Distribution") +
  theme(text=element_text(size=25))


#Multivariate #Scatter Plot
ggplot(df, aes_string(x = df$month, y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Year") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=12))

################# Date ###########################


ggplot(df, aes_string(x = df$day)) + 
  geom_histogram(fill="cornsilk", colour = "black", bins = 61) + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=32))+
  theme_bw() + xlab("Date") + ylab("Frequency") + ggtitle("Datewise Distribution") +
  theme(text=element_text(size=25)) 


#Multivariate #Scatter Plot
ggplot(df, aes_string(x = df$day, y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Date") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=25))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=30))

############### Day of week ######################


ggplot(df, aes_string(x = df$dayofweek)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=7))+
  theme_bw() + xlab("Day") + ylab("Frequency") + ggtitle("Daywise Distribution") +
  theme(text=element_text(size=20))


#Multivariate #Scatter Plot
ggplot(df, aes_string(x = df$dayofweek, y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Day") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=20))+scale_y_continuous(breaks=pretty_breaks(n=7)) + 
  scale_x_continuous(breaks=pretty_breaks(n=7))

################## Time of day ########################


ggplot(df, aes(x = factor(df$timeofday))) +
  geom_bar(fill = "light blue") +
  theme_classic()+ xlab("Time of Day") + ylab("Frequency") + ggtitle("Hourwise Distribution") +
  theme(text=element_text(size=20))


#Multivariate #Scatter Plot
ggplot(df, aes(x =factor(df$timeofday) , y = df$fare_amount)) + 
  geom_point(aes_string(colour = df$fare_amount),size = 4) +
  theme_bw()+ ylab("Fare amount") + xlab("Time of Day") + ggtitle("Scatter plot Analysis") + 
  theme(text=element_text(size=20))


summary(df)

write.csv(df, "train_clean.csv", row.names = F)


 




