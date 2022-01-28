# ---
# title: "IST 687 Project Group 3 Spring 2021"
# author: "Abhijit Gokhale, Srishti Sanghvi, Ruixue Qin, Mackenzie Ess, Thejaswini Ram, Kevin Chung"
# Attribution statement: # We attempted this code with our own efforts. At some points we took help from the 
                         # homework assignments and book.
# ---

###### set the file directory ######
getwd()

setwd("C:\\Users\\abhij\\Desktop\\Lectures\\SEM 1\\IST 687\\Project")

# install.packages("readxl")
library(readxl)
## Reading the data files  ##
dataH1 <- read_excel("H1-Resort.xlsx")

dataH2 <- read_excel("H2-City.xlsx")



###### checking data set structure ######
str(dataH1)

str(dataH2)

# Both datasets are large in numbers and have numerous columns. The columns that we are focusing on are ADR, IsCanceled.

## using psych library to use describe function 
# install.packages("psych")
library(psych)

describe(dataH1)

describe(dataH2)

# The describe function shows in depth descriptive statistics on the data set.

###### Clean the data ######
# install.packages("imputeTS")
# install.packages("dlookr")
library(imputeTS)
library(dlookr)

# checking for NA values in the dataset H1 AND H2
colSums(is.na(dataH1))

colSums(is.na(dataH2))

# H1 - Resort #

# changing the NA values in Agent by its NA interpolated values.
dataH1$Agent <-  as.numeric(dataH1$Agent)
dataH1$Agent <- na_interpolation(dataH1$Agent)

#dataH1$Company <- nulltona(dataH1$Company) # not considering Company in the model as it has more than 90% values NA

# H2 - City #

# changing the NA values in Agent by its NA interpolated values.
dataH2$Agent <-  as.numeric(dataH2$Agent)
dataH2$Agent <- na_interpolation(dataH2$Agent)

#dataH2$Company <- nulltona(dataH2$Company) # not considering Company in the model as it has more than 90% values NA


# Removing 4 NA values from Children column in H2-city #

dataH2 <- subset(dataH2, Children!= "NA")

# Converting Children column from character to numeric in H2-City #

dataH2$Children <- as.numeric(dataH2$Children)


# Renaming the Arrival Date column to Arrival_Date for H1 - Resort and H2-City datasets #
colnames(dataH1)[colnames(dataH1) == "Arrival Date"] <- "Arrival_Date"

colnames(dataH2)[colnames(dataH2) == "Arrival Date"] <- "Arrival_Date"



### Diagnosing Outliers and Special characters in the dataset ###

# H1- Resort taking care of special character ` #

dataH1[dataH1$"Arrival_Date"< dataH1$ReservationStatusDate & dataH1$ReservationStatus!= "Check-Out",]$ReservationStatus <- "Check-Out"

# H2 - City taking care of outlier ADR = 5400 #

dataH2<-subset(dataH2, ADR!= 5400)


colSums(is.na(dataH1))
colSums(is.na(dataH2))


# Removing Nulls values present under column Country from both datasets

dataH1 <- subset(dataH1, Country!= "NULL")
dataH2 <- subset(dataH2, Country!= "NULL")

# checking null values in the Country column
table(dataH1$Country == "NULL")

table(dataH2$Country == "NULL")




# Using dlookr package to diagnose the outliers present in the dataset H1- Resort with the help of diagnose_outlier() function #

diagnose_outlier(dataH1)

# Using dlookr package to diagnose the outliers present in the dataset H2- City with the help of diagnose_outlier() function #

diagnose_outlier(dataH2)


# Explanation : In the above results, we can see that both datasets have many outliers in them. There are several reasons why we
# thought of not removing the outliers, firstly since the outliers are many it is possible that they will help us to build a better and bias free model.
# Secondly the outliers may help us in understanding the variablity in the data and help us explore why the ADR is varying based on the non numerical and numerical data.
# as ADR values in different seasons and under different categories. As they are present large in numbers, in this case it makes sense 
# to study outliers and central values separately, but there's no reason to assume one is more important than the other. 
# In many cases outliers are more consequential in aggregate than central values, despite being rare.






###### Add new columns to the data as per business requirement ######

## Created new column visitor_type##
# H1 - Resort #
dataH1$visitor_type[(dataH1$Babies>= 0 | dataH1$Children>= 0)& dataH1$Adults > 0] <- "Family"

dataH1$visitor_type[(dataH1$Adults == 2)] <- "Couple" 

dataH1$visitor_type[(dataH1$Adults == 1)] <- "Single" 

sum(table(dataH1$visitor_type))

# For H1 - Resort, below code shows 13 NA records in visitor_type because there are 0 adults, children and babies in the respective columns. 
# So we should remove them because we are not considering records where only children are going to stay in the hotel without parents(Adults)
View(dataH1[is.na(dataH1$visitor_type) == TRUE,])


# H2 - City #
dataH2$visitor_type[(dataH2$Babies>= 0 | dataH2$Children>= 0) & dataH2$Adults > 0] <- "Family"

dataH2$visitor_type[(dataH2$Adults == 2)] <- "Couple" 

dataH2$visitor_type[(dataH2$Adults == 1)] <- "Single" 

sum(table(dataH2$visitor_type))

# For H2 - City below code shows 390 NA records in visitor_type because there are 0 adults and babies in the respective columns. 
# So we should remove them because we are not considering records where only children are going to stay in the hotel without parents(Adults)
View(dataH2[is.na(dataH2$visitor_type) == TRUE,])


# Removing NA's from visitor_type

dataH1 <- subset(dataH1, visitor_type!= "NA")
dataH2 <- subset(dataH2, visitor_type!= "NA")



## created new column season ##

# Created below function to extract month value from the ReservationStatusDate     
extractmonth = function (date) {
  month = format(date, format="%m")
  year = format(date, format="%Y")
  #monthyear = c(month, year)
  #list(day=day, month=month, year=year)
  return (month)
}

# For H1 - Resort Data # 

# converting the returned month value to numeric so that it can be compared later in the code
monthonlyH1 <- as.numeric(extractmonth(dataH1$ReservationStatusDate))  

dataH1$season[monthonlyH1>=1 & monthonlyH1<=3] <- "Spring" 
dataH1$season[monthonlyH1>=4 & monthonlyH1<=6] <- "Summer" 
dataH1$season[monthonlyH1>=7 & monthonlyH1<=9] <- "Fall" 
dataH1$season[monthonlyH1>=10 & monthonlyH1<=12] <- "Winter"

sum(table(dataH1$season)) #checking whether seasons value got applied to all the records in the data

# For H2 - City Data #

# converting the returned month value to numeric so that it can be compared later in the code
monthonlyH2 <- as.numeric(extractmonth(dataH2$ReservationStatusDate))

dataH2$season[monthonlyH2>=1 & monthonlyH2<=3] <- "Spring" 
dataH2$season[monthonlyH2>=4 & monthonlyH2<=6] <- "Summer" 
dataH2$season[monthonlyH2>=7 & monthonlyH2<=9] <- "Fall" 
dataH2$season[monthonlyH2>=10 & monthonlyH2<=12] <- "Winter"

sum(table(dataH2$season)) #checking whether seasons value got applied to all the records in the data


## created new column AVGrevperstay ##

# For H1 - Resort Data #
dataH1$AVGrevperstay <- (dataH1$StaysInWeekendNights + dataH1$StaysInWeekNights) * dataH1$ADR

# For H2 - City Data #
dataH2$AVGrevperstay <- (dataH2$StaysInWeekendNights + dataH2$StaysInWeekNights) * dataH2$ADR

## Created New Column roomtypechanged ##

# For H1 - Resort Data #
dataH1$roomtypechanged[dataH1$ReservedRoomType == dataH1$AssignedRoomType] <- "No Change"
dataH1$roomtypechanged[dataH1$ReservedRoomType != dataH1$AssignedRoomType] <- "Change"


# For H2 - City Data #
dataH2$roomtypechanged[dataH2$ReservedRoomType == dataH2$AssignedRoomType] <- "No Change"
dataH2$roomtypechanged[dataH2$ReservedRoomType != dataH2$AssignedRoomType] <- "Change"


# Checking for NA values in the datasets after creating new Columns
colSums(is.na(dataH1))
colSums(is.na(dataH2))



# Removing columns such as Arrival_Date, Company and ReservationStatusdate, ReservedRoomType and AsignedRoomType from both datasets because 
# the date part is not going to contribute in our linear model. Secondly, as assigned room type and reserved room type do not tell us 
# whether change in room type is hotel staff decision or customer decision. So we can not decide whether it is good thing and helping 
# to generate better ADR or not.

dataH1.1 <- dataH1[,!colnames(dataH1) %in% c("Arrival_Date","ReservationStatusDate","AssignedRoomType","ReservedRoomType","Company") ]

dataH2.1 <- dataH2[,!colnames(dataH2) %in% c("Arrival_Date","ReservationStatusDate","AssignedRoomType","ReservedRoomType","Company") ]


# Checking for NA values in the datasets after creating new Columns

colSums(is.na(dataH1.1))
colSums(is.na(dataH2.1))






###### Vector for Non-Numerical columns ######


datacols <- c("ReservationStatus","Country", "Meal","MarketSegment","DistributionChannel","DepositType","CustomerType","visitor_type","season","roomtypechanged")

# Creating countries table to present which countries are contributing more in our H1 - Resort dataset data #
countryH1<- as.data.frame(table(dataH1.1$Country))

countryH1$Country_code = countryH1$Var1

countryH1$Var1 = NULL

countryH1$count <- countryH1$Freq

countryH1$Freq = NULL

countryH1 <- countryH1[order(-countryH1$count),]

#Storing_Top_10 recorded countries from H1 in the variable 

toprecordedcountryH1 <- countryH1[1:10,]

# Among all the countries, PRT has maximum records 17622 then comes GBR with 6813 records, ESP with 3956 records, 
# IRL with 2166 records, FRA with 1610 records and DEU with 1203 records. This tells us that PRT is contributing highest 
# to get the ADR values.   

# Creating countries table to present which countries are contributing more in our H1 - Resort dataset data #
countryH2<- as.data.frame(table(dataH2.1$Country))

countryH2$Country_code = countryH2$Var1

countryH2$Var1 = NULL

countryH2$count <- countryH2$Freq

countryH2$Freq = NULL

countryH2 <- countryH2[order(-countryH2$count),]

#Storing_Top_10 recorded countries from H2 in the variable 

toprecordedcountryH2 <- countryH2[1:10,]

# Among all the countries, PRT has maximum records 30813 then comes GBR with 6813 records, FRA with 8766 records, 
# DEU with 6068 records, GBR with 5292 records, ESP with 4590 records and ITA 3293 records. This tells us that PRT is 
# contributing highest to get the ADR values. 


## Plotting the World-heatmap for those countries contributing the most to H1 - Resort##

# install.packages("ggplot2")
# install.packages("maps")
library(ggplot2)
library(maps)

WorldData <- map_data('world')
head(WorldData, 100)

## storing the countries with maximum reservation data in countryH1 ##
countryH1 <- data.frame(region = c("Portugal", "United Kingdom", "Spain", "Irland", "France", "Germany", "China", "Netherlands", "United States", "Italy"), value= toprecordedcountryH1$count)

## Filtering the regions from the world data to those matching with our requirement ##
H1map <- WorldData[WorldData$region %in% countryH1$region, ]
H1map$value <- countryH1$value[match(H1map$region, countryH1$region)]

# Plotting the map #
ggplot(H1map, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_map(map = WorldData, aes(map_id = region))+
  geom_polygon(colour = "black") +
  scale_fill_continuous(low = "thistle2", 
                        high = "darkred", 
                        guide="colorbar") +
  theme_bw()  + 
  labs(fill = "Number of Reservations" ,title = "Top recorded Countries for H1-Resort Data", x="", y="") +
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) 


# Another way Plotting the countries contribution for H1- resort using dot map #
ggplot(data = toprecordedcountryH1, aes(x = Country_code , y = count)) + geom_point(aes(size = count), alpha = I(5), color = "orange")


## Plotting the World-heatmap for those countries contributing the most to H2 - City##  
library(ggplot2)
library(maps)

WorldData <- map_data('world')
head(WorldData, 100)

## storing the countries with maximum reservation data in countryH2 ##
countryH2 <- data.frame(region = c("Portugal", "France", "Germany", "United Kingdom", "Spain", "Italy", "Belgium", "Brazil", "United States", "Netharlands"), value= toprecordedcountryH2$count)

## Filtering the regions from the world data to those matching with our requirement ##
H2map <- WorldData[WorldData$region %in% countryH2$region, ]
H2map$value <- countryH2$value[match(H2map$region, countryH2$region)]

# Plotting the map #
ggplot(H2map, aes(x=long, y=lat, group = group, fill = value)) + 
  geom_map(map = WorldData, aes(map_id = region))+
  geom_polygon(colour = "black") +
  scale_fill_continuous(low = "thistle2", 
                        high = "darkred", 
                        guide="colorbar") +
  theme_bw()  + 
  labs(fill = "Number of Reservations" ,title = "Top recorded Countries for H2-City Data", x="", y="") +
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) 



# Another way of plotting the countries contribution for H2- city using dot map #
ggplot(data = toprecordedcountryH2, aes(x = Country_code , y = count)) + geom_point(aes(size = count), alpha = I(5), color = "orange")


###### Separating Numerical Data ## Note: !colnames(dataH1)  The ! won't include the selected columns  ######  

Numerical_dataH1 <- dataH1.1[,!colnames(dataH1.1) %in% datacols ]

Numerical_dataH2 <- dataH2.1[,!colnames(dataH2.1) %in% datacols ]

# checking the descriptive statistics for Numerical data from both datasets 

summary(Numerical_dataH1)

summary(Numerical_dataH2)


###### Histograms Of Numerical Data ######  

#H1: Resort
library(ggplot2)
# Only two distinct values exist of either 0 or 1. The value of 0 has a greater frequency than the value of 1.
ggplot(Numerical_dataH1, aes(x=IsCanceled)) + geom_histogram()
# Lead time values are distributed and range from approximately 0 to 400 days.
ggplot(Numerical_dataH1, aes(x=LeadTime)) + geom_histogram()
# It can be observed that majority of people stayed for less than 5 weekend nights and most between 0-2 weekend nights only
ggplot(Numerical_dataH1, aes(x=StaysInWeekendNights)) + geom_histogram()
# It can be observed that majority of people stayed for less than 10 week nights and most stayed for either 1 or 5 week nights
ggplot(Numerical_dataH1, aes(x=StaysInWeekNights)) + geom_histogram()
# The number of adults that stayed at the resort varied from 1 to 2, with majority being 2 adults
ggplot(Numerical_dataH1, aes(x=Adults)) + geom_histogram(bins=10)
# The number of children that stayed at the resort varied from 0 to 10, with mostly being 0
ggplot(Numerical_dataH1, aes(x=Children)) + geom_histogram(bins=40)
# The number of babies that stayed at the resort varied from 0 - 2, but were mostly 0
ggplot(Numerical_dataH1, aes(x=Babies)) + geom_histogram(bins=40)
# The number of repeated guests varied from 0 to 1, but mostly were 0
ggplot(Numerical_dataH1, aes(x=IsRepeatedGuest)) + geom_histogram()
# The number of previous cancellations ranged from 0 - 26, but mostly were 0
ggplot(Numerical_dataH1, aes(x=PreviousCancellations)) + geom_histogram()
# Previous bookings not cancelled varied betweent 0 - 10, but mostly were 0
ggplot(Numerical_dataH1, aes(x=PreviousBookingsNotCanceled)) + geom_histogram()
# Number of booking changes varied betweent 0 - 3, but mostly were 0
ggplot(Numerical_dataH1, aes(x=BookingChanges)) + geom_histogram()
# Agent IDs varied signficantly and had a stretched distribution
ggplot(Numerical_dataH1, aes(x=Agent)) + geom_histogram()
# Most number of days in waiting list were 0
ggplot(Numerical_dataH1, aes(x=DaysInWaitingList)) + geom_histogram()
# Most ADR values varied between  0 to 300
ggplot(Numerical_dataH1, aes(x=ADR)) + geom_histogram(bins = 5)
# Varied from 0 to 1, but were mostly 0
ggplot(Numerical_dataH1, aes(x=RequiredCarParkingSpaces)) + geom_histogram()
# Varied from 0 to 8, but were most 0
ggplot(Numerical_dataH1, aes(x=TotalOfSpecialRequests)) + geom_histogram()
# Varied between 0 and 300, most stayed below $2000.
ggplot(Numerical_dataH1, aes(x=AVGrevperstay)) + geom_histogram(bins=10)



#H2: City
library(ggplot2)
# Only two distinct values exist of either 0 or 1. The value of 0 has a greater frequency than the value of 1.
ggplot(Numerical_dataH2, aes(x=IsCanceled)) + geom_histogram()
# Lead time values are distributed and range from approximately 0 to 500 days. Mostly 0 days 
ggplot(Numerical_dataH2, aes(x=LeadTime)) + geom_histogram()
# It can be observed that majority of people stayed for less than 5 weekend nights and most between 0-1 weekend nights only
ggplot(Numerical_dataH2, aes(x=StaysInWeekendNights)) + geom_histogram()
# It can be observed that majority of people stayed for less than 10 week nights and most stayed for either 1 or 2 week nights
ggplot(Numerical_dataH2, aes(x=StaysInWeekNights)) + geom_histogram()
# The number of adults that stayed at the resort varied from 1 to 4, with majority being 2 adults
ggplot(Numerical_dataH2, aes(x=Adults)) + geom_histogram(bins=10)
# The number of children that stayed at the resort varied from 0 to 10, with mostly being 0
ggplot(Numerical_dataH2, aes(x=Children)) + geom_histogram(bins=40)
# The number of babies that stayed at the resort varied from 0 - 2, but were mostly 0
ggplot(Numerical_dataH2, aes(x=Babies)) + geom_histogram(bins=40)
# The number of repeated guests varied from 0 to 1, but mostly were 0
ggplot(Numerical_dataH2, aes(x=IsRepeatedGuest)) + geom_histogram()
# The number of previous cancellations ranged from 0 - 21, but mostly were 0
ggplot(Numerical_dataH2, aes(x=PreviousCancellations)) + geom_histogram()
# Previous bookings not canceled were mostly 0
ggplot(Numerical_dataH2, aes(x=PreviousBookingsNotCanceled)) + geom_histogram()
# Number of booking changes varied between 0 - 3, but mostly were 0
ggplot(Numerical_dataH2, aes(x=BookingChanges)) + geom_histogram()
# Agent IDs varied signficantly and had a stretched distribution
ggplot(Numerical_dataH2, aes(x=Agent)) + geom_histogram()
# Most number of days in waiting list were 0
ggplot(Numerical_dataH2, aes(x=DaysInWaitingList)) + geom_histogram()
# Most ADR values varied between  0 to 300
ggplot(Numerical_dataH2, aes(x=ADR)) + geom_histogram(bins = 5)
# Varied from 0 to 1, but were mostly 0
ggplot(Numerical_dataH2, aes(x=RequiredCarParkingSpaces)) + geom_histogram()
# Varied from 0 to 5, but were mostly 0 or 1
ggplot(Numerical_dataH2, aes(x=TotalOfSpecialRequests)) + geom_histogram()
# Varied between 0 and 300, most stayed below $2000.
ggplot(Numerical_dataH2, aes(x=AVGrevperstay)) + geom_histogram(bins=10)


# Findings and adjustments needed in H1-Resort based on histograms data #
for (i in c(1:ncol(Numerical_dataH1))) hist(Numerical_dataH1[[i]],main = colnames(Numerical_dataH1)[i],xlab = colnames(Numerical_dataH1)[i])

######## Below are the decisions taken to change the dataset before entering into data modeling and data mining techniques ######

# 1) DaysInWaitingList has no weightage to the model, because more than 90% records have value 0.
# 2) BookingChanges has no weightage to the model, because more than 90% records have value 0.
# 3) PreviousCancellations and PreviousBookingsNotCanceled have no weightage to the model, because more than 90% records have value 0.
# 4) IsRepeatedGuest should be converted to factor or character datatype
# 5) Babies has no weightage to the model, because more than 90% records have value 0.
# 6) LeadTime has no direct relation with ADR value as can be seen from below plots
# 7) IsCanceled should be converted to factor or character datatype
# 8) RequiredCarParkingSpaces has no weightage to the model, because more than 85% records have value 0.



# Findings and adjustments needed in H2-City based on histograms data #
for (i in c(1:ncol(Numerical_dataH2))) hist(Numerical_dataH2[[i]],main = colnames(Numerical_dataH2)[i],xlab = colnames(Numerical_dataH2)[i])

######## Below are the decisions taken to change the dataset before entering into data modeling and data mining techniques: ######

# 1) DaysInWaitingList has no weightage to the model, because more than 90% records have value 0.
# 2) BookingChanges has no weightage to the model, because more than 90% records have value 0.
# 3) PreviousCancellations and PreviousBookingsNotCanceled have no weightage to the model, because more than 90% records have value 0.
# 4) IsRepeatedGuest should be converted to factor or character datatype
# 5) Babies has no weightage to the model, because more than 90% records have value 0.
# 6) LeadTime has no direct relation with ADR value as can be seen from below plots
# 7) IsCanceled should be converted to factor or character datatype
# 8) RequiredCarParkingSpaces has no weightage to the model, because more than 85% records have value 0
# 9) IsrepeatedGuest and Is_canceled can be involved in arules to get a general idea about associations.





###### Separating Non-Numerical Data ######

NonNumerical_dataH1 <- dataH1.1[,colnames(dataH1.1) %in% datacols ]

NonNumerical_dataH2 <- dataH2.1[,colnames(dataH2.1) %in% datacols ]


####### barplots ######

library(ggplot2)

### For H1 - Resort Data ###


ggplot(dataH1.1, aes(as.factor(Meal), ADR, fill = as.factor(IsCanceled))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")+xlab("Meal Type") + labs(fill = "Is Canceled")

# For meal type FB we are getting more ADR for canceled records. As the ADR values for canceled records and non canceled records
# for HB meal type are very close, we should focus on improving HB meal types along with other meal types 
# to improve the ADR.

ggplot(dataH1.1, aes(as.factor(Meal), ADR, fill = as.factor(ReservationStatus))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")+xlab("Meal Type") + labs(fill = "Reservation Status")
# with BB as meal type there are more ADR values with no show and canceled records

ggplot(dataH1.1, aes(as.factor(MarketSegment), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Market Segment") + labs(fill = "Is Canceled")
# With Market segment as online TA and Direct we have higher ADR values for canceled records. Corporate market segment 
# people are contributing with higher ADR values for more checkouts than other marker segments.


ggplot(dataH1.1, aes(as.factor(MarketSegment), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Market Segment") + labs(fill = "Reservation Status")
# People are not showing up for their reservations with online TA as market segment.

ggplot(dataH1.1, aes(as.factor(DistributionChannel), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Distribution Channel") + labs(fill = "Is Canceled")
# The undefined distribution channel is contributing in ADR with all customers without cancellations.
# Corporate distribution channel is also contributing in high ADR values for customers without cancellations.
# We are getting cancellations where distribution channel is Direct and TA/TO.


ggplot(dataH1.1, aes(as.factor(DistributionChannel), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Distribution Channel") + labs(fill = "Reservation Status")
# ADR values are high for customers not showing up where Distribution channel is TA/TO.


ggplot(dataH1.1, aes(as.factor(DepositType), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Deposit Type") + labs(fill = "Is Canceled")
# For no deposit category customer records are present with high ADR values without cancellations.
# Whether deposit type is non refundable or refundable we are getting high ADR values for cancellations  

ggplot(dataH1.1, aes(as.factor(DepositType), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Deposit Type") + labs(fill = "Reservation Status")
# We are also getting high ADR values for not showing customer records for no deposit type.


ggplot(dataH1.1, aes(as.factor(CustomerType), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Customer Type") + labs(fill = "Is Canceled")
# For the customer type transient, we are getting higher ADR values without cancellations and overall canceled records are more in transient type as well
# In the same way, for other customer type categories, we are having higher ADR values without cancellations than canceled records.

ggplot(dataH1.1, aes(as.factor(CustomerType), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Customer Type") + labs(fill = "Reservation Status")
# we are getting high ADR values for Transient type as well.

ggplot(dataH1.1, aes(as.factor(visitor_type), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Visitor Type") + labs(fill = "Is Canceled")
# we are getting high ADR without cancellations where Vistor type is Couple.

ggplot(dataH1.1, aes(as.factor(visitor_type), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Visitor_Type") + labs(fill = "Reservation Status")
# For visitor type as family, we are geting high ADR with family not showing up.


ggplot(dataH1.1, aes(as.factor(season), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("season") + labs(fill = "Is Canceled")
# In fall season the ADR values are highest and no cancellations. Whereas, for all other categories, the ADR values are highest 
# with cancellations.

ggplot(dataH1.1, aes(as.factor(season), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("season") + labs(fill = "Reservation Status")
# Also, ADR values are high because customers are not showing up in Fall season.

ggplot(dataH1.1, aes(as.factor(roomtypechanged), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("was room type changed?") + labs(fill = "Is Canceled")
# If the room type is changed, then we have high ADR values and no cancellations.
# If the room type is not changed , then we have high ADR values with cancellations.


ggplot(dataH1.1, aes(as.factor(roomtypechanged), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("was room type changed?") + labs(fill = "Reservation Status")
# If the room type is not changed , then we have high ADR values with cancellations and no show.


### H2 - City ###


ggplot(dataH2.1, aes(as.factor(Meal), ADR, fill = as.factor(IsCanceled))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")+xlab("Meal Type") + labs(fill = "Is Canceled")
# ADR value is highest without cancellations for BB as meal type.
# ADR value is highest with cancellations for sc as meal type.

ggplot(dataH2.1, aes(as.factor(Meal), ADR, fill = as.factor(ReservationStatus))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2")+xlab("Meal Type") + labs(fill = "Reservation Status")
# ADR values is highest with customers not showing up and meal type as HB.

ggplot(dataH2.1, aes(as.factor(MarketSegment), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Market Segment") + labs(fill = "Is Canceled")
# ADR values are higher for Direct and offline TA/TO market segment without cancellations.
# ADR values are highest for online TA as market segment with cancellations. Also, for corporate Market segment
# ADR values are affected because more records are canceled.


ggplot(dataH2.1, aes(as.factor(MarketSegment), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Market Segment") + labs(fill = "Reservation Status")
# ADR values are same for all three categories for Groups market segment. Also for online TA market category
# we have more ADR values overall where customers are no showing up.

ggplot(dataH2.1, aes(as.factor(DistributionChannel), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Distribution Channel") + labs(fill = "Is Canceled")
# ADR values are affected positively without cancellations for Direct and TA/TO as Distribution channel.
# ADR values are affected inversely highest for TA/TO distribution channel. In corporate and GDS distribution channels,
# more ADR values are affected inversely.


ggplot(dataH2.1, aes(as.factor(DistributionChannel), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Distribution Channel") + labs(fill = "Reservation Status")
# ADR values got affected because customers are not showing up with TA/TO and GDS distribution channel.

ggplot(dataH2.1, aes(as.factor(DepositType), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Deposit Type") + labs(fill = "Is Canceled")
# ADR values are highest and no cancellations where the reservation is done with no deposit. 
# Whether deposit type is non refundable or refundable we are getting high ADR values for cancellations.

ggplot(dataH2.1, aes(as.factor(DepositType), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Deposit Type") + labs(fill = "Reservation Status")
# ADR values are affected with customers not showing up when there is no deposit. Also ADR values are affected 
# with more cancellations and there is no case where customers are not showing up. 

ggplot(dataH2.1, aes(as.factor(CustomerType), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Customer Type") + labs(fill = "Is Canceled")
# transient and transient party are giving out high ADR values without cancellations. Also we are getting high 
# cancellations overall where transient is category.

ggplot(dataH2.1, aes(as.factor(CustomerType), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Customer Type") + labs(fill = "Reservation Status")
# ADR values are getting affcted with people not showing up in transient category.

ggplot(dataH2.1, aes(as.factor(visitor_type), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Visitor Type") + labs(fill = "Is Canceled")
# Single customers are contributing more in giving higher ADR values without cancellations. 
# Couples are contributing in giving higher ADR values in general with cancellations.

ggplot(dataH2.1, aes(as.factor(visitor_type), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("Visitor Type") + labs(fill = "Reservation Status")
# Couples are not showing up and giving higher ADR values overall.

ggplot(dataH2.1, aes(as.factor(season), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("season") + labs(fill = "Is Canceled")
# In summers we are getting highest ADR without cancellations. Also in Fall, we are getting higher ADR values 
# with cancellations in general.

ggplot(dataH2.1, aes(as.factor(season), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("season") + labs(fill = "Reservation Status")
# Customers which are not showing up in general are also contributing in higher ADR values.

ggplot(dataH2.1, aes(as.factor(roomtypechanged), ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("was room type changed?") + labs(fill = "Is Canceled")
# we have high ADR values without cancellations for both when there is a change in room type and 
# when there is no change in room type

ggplot(dataH2.1, aes(as.factor(roomtypechanged), ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("was room type changed?") + labs(fill = "Reservation Status")
# customers tend to not show up more and affecting ADR when the room type is not changed.



###### Keeping ADR and Categorical or Character Data in one dataset to create different boxplot ######

bxplotdataH1 <- dataH1.1[,colnames(dataH1.1) %in% c("ADR",datacols) ]

bxplotdataH2 <- dataH2.1[,colnames(dataH2.1) %in% c("ADR",datacols) ]


## Exploring Boxplots ##

# Numeric Boxplots: Used for analysis of outliers
#H1: Resort
library(ggplot2)
# No outliers
boxplot(Numerical_dataH1$IsCanceled)
# Outliers beyond 400 days
boxplot(Numerical_dataH1$LeadTime)
# Few outliers beyond 5 days
boxplot(Numerical_dataH1$StaysInWeekendNights)
# Few outliers beyond 10 days
boxplot(Numerical_dataH1$StaysInWeekNights)
# Few outliers beyond 2 adults
boxplot(Numerical_dataH1$Adults)
# Few outliers beyond 0 children
boxplot(Numerical_dataH1$Children)
# 2 outliers beyond 0 babies
boxplot(Numerical_dataH1$Babies)
# Only one outlier as 1 repeated guest
boxplot(Numerical_dataH1$IsRepeatedGuest)
# Few outliers beyond 0 previous cancellations
boxplot(Numerical_dataH1$PreviousCancellations)
# Quite a few outliers beyond 0
boxplot(Numerical_dataH1$PreviousBookingsNotCanceled)
# Outliers beyond 0
boxplot(Numerical_dataH1$BookingChanges)
# Agent IDs ranged from 200 - 300
boxplot(Numerical_dataH1$Agent)
# Outliers beyond 0
boxplot(Numerical_dataH1$DaysInWaitingList)
# Outlier values beyond 200. The ADR ranges from 50 to 150.
boxplot(Numerical_dataH1$ADR)
# Few outlier values beyond 0
boxplot(Numerical_dataH1$RequiredCarParkingSpaces)
# ranges from 0 to 1, few outlier values beyond 2
boxplot(Numerical_dataH1$TotalOfSpecialRequests)
# Few outliers beyond $1500.
boxplot(Numerical_dataH1$AVGrevperstay)

#H2: City
library(ggplot2)
# No outliers
boxplot(Numerical_dataH2$IsCanceled)
# Outliers beyond 400 days
boxplot(Numerical_dataH2$LeadTime)
# Few outliers beyond 5 days
boxplot(Numerical_dataH2$StaysInWeekendNights)
# Few outliers beyond 6 days
boxplot(Numerical_dataH2$StaysInWeekNights)
# Few outliers beyond 2 adults
boxplot(Numerical_dataH2$Adults)
# Few outliers beyond 2 children
boxplot(Numerical_dataH2$Children)
# 2 outliers beyond 0 babies
boxplot(Numerical_dataH2$Babies)
# Only one outlier as 1 repeated guest
boxplot(Numerical_dataH2$IsRepeatedGuest)
# Few outliers beyond 0 previous cancellations
boxplot(Numerical_dataH2$PreviousCancellations)
# Quite a few outliers beyond 0
boxplot(Numerical_dataH2$PreviousBookingsNotCanceled)
# Outliers beyond 0
boxplot(Numerical_dataH2$BookingChanges)
# Agent IDs ranged from 0 - 50
boxplot(Numerical_dataH2$Agent)
# Outliers beyond 0
boxplot(Numerical_dataH2$DaysInWaitingList)
# Outlier values beyond 200. The ADR ranges from 50 to 150.
boxplot(Numerical_dataH2$ADR)
# Few outlier values beyond 0
boxplot(Numerical_dataH2$RequiredCarParkingSpaces)
# ranges from 0 to 1, few outlier values beyond 2
boxplot(Numerical_dataH2$TotalOfSpecialRequests)
# Few outliers beyond $500.
boxplot(Numerical_dataH2$AVGrevperstay)

# Analyzing variability in ADR values for all the categories in the dataset
# ADR & Meal #
boxplot(ADR~Meal, data = bxplotdataH1) #plenty of outliers for all type,SC have a smaller medium compare to others which have similar medium
boxplot(ADR~Meal, data = bxplotdataH2)#very few data fro FB and lots of outliers for BB and HB

# ADR & Customer Type #
boxplot(ADR~CustomerType, data = bxplotdataH1)#way more data for transient and transient-party
boxplot(ADR~CustomerType, data = bxplotdataH2)#similar medium for all type but more data for transient and transient-party

# ADR & RoomType #
boxplot(ADR~roomtypechanged, data = bxplotdataH1)#mo change is more frequent than change 
boxplot(ADR~roomtypechanged, data = bxplotdataH2)#similar medium for change and no change

# ADR & ReservationStatus #
boxplot(ADR~ReservationStatus, data = bxplotdataH1)#more outliers for check out and canceled
boxplot(ADR~ReservationStatus, data = bxplotdataH2)  #similar medium for all status

# ADR & Country #
boxplot(ADR~ReservationStatus, data = bxplotdataH1)#more outliers for check out and canceled
boxplot(ADR~ReservationStatus, data = bxplotdataH2)  #similar medium for all status

# ADR & MarketSegment #
boxplot(ADR~MarketSegment, data = bxplotdataH1)# more outliers for direct and online TA
boxplot(ADR~MarketSegment, data = bxplotdataH2)  #more outliers for online TA

# ADR & DistributionChannel #
boxplot(ADR~DistributionChannel, data = bxplotdataH1)#plenty of outliers for all type 
boxplot(ADR~DistributionChannel, data = bxplotdataH2)  #plenty of outliers for direct and TA/TO
# ADR & ReservedRoomType #
boxplot(ADR~ReservedRoomType, data = bxplotdataH1)#plenty of outliers for all type except b and l
boxplot(ADR~ReservedRoomType, data = bxplotdataH2)  #plenty of outliers for all type except p
# ADR & AssignedRoomType #
boxplot(ADR~AssignedRoomType, data = bxplotdataH1)#plenty of data for all type except l and p
boxplot(ADR~AssignedRoomType, data = bxplotdataH2) #plenty of outliers for all type except p
# ADR & DepositType #
boxplot(ADR~DepositType, data = bxplotdataH1)#more outliers for no deposite
boxplot(ADR~DepositType, data = bxplotdataH2)  #more outliers for no deposite





######################################################################
#Association Rules Mining 

#install.packages("arules")
#installing arules package
#install.packages("arulesViz")
#installing arulesViz package
library(arules)
#loading arules library
library(arulesViz)
#loading arulesViz library

################################   H1 data cleaning
View(dataH1)

# recode continuous variables to dummy variables
dataH1$Babies[dataH1$Babies >= 2] <- 2
dataH1$Children[dataH1$Children >= 2] <- 2
dataH1$Adults[dataH1$Adults >= 2] <- 2

dataH1_new <- data.frame(IsRepeatedGuest=as.factor(dataH1$IsRepeatedGuest),
                         IsCanceled=as.factor(dataH1$IsCanceled),
                         Adults=as.factor(dataH1$Adults),
                         Children=as.factor(dataH1$Children),
                         Babies=as.factor(dataH1$Babies),
                         Meal=as.factor(dataH1$Meal),
                         Country=as.factor(dataH1$Country),
                         MarketSegment=as.factor(dataH1$MarketSegment),
                         ReservedRoomType=as.factor(dataH1$ReservedRoomType),
                         Agent=as.factor(dataH1$Agent),
                         Company=as.factor(dataH1$Company),
                         CustomerType=as.factor(dataH1$CustomerType))

################################   H2 data cleaning
View(dataH2)


# recode continuous variables to dummy variables
dataH2$Babies[dataH2$Babies >= 2] <- 2
dataH2$Children[dataH2$Children >= 2] <- 2
dataH2$Adults[dataH2$Adults >= 2] <- 2

dataH2_new <- data.frame(IsRepeatedGuest=as.factor(dataH2$IsRepeatedGuest),
                         IsCanceled=as.factor(dataH2$IsCanceled),
                         Adults=as.factor(dataH2$Adults),
                         Children=as.factor(dataH2$Children),
                         Babies=as.factor(dataH2$Babies),
                         Meal=as.factor(dataH2$Meal),
                         Country=as.factor(dataH2$Country),
                         MarketSegment=as.factor(dataH2$MarketSegment),
                         ReservedRoomType=as.factor(dataH2$ReservedRoomType),
                         Agent=as.factor(dataH2$Agent),
                         Company=as.factor(dataH2$Company),
                         CustomerType=as.factor(dataH2$CustomerType))


################################   H1 ARM

levels(dataH1_new$IsCanceled)
levels(dataH1_new$IsRepeatedGuest)
#getting levels for factor variables


data1x <- as(dataH1_new, "transactions")
#coercing data1x into a transactions matrix

rules1 <- apriori(data1x, 
                  parameter=list(supp=0.5, conf=0.55), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("IsCanceled=0")))
#defined the rules to look for associations with rows in which people did not cancel

rules2 <- apriori(data1x, 
                  parameter=list(supp=0.5, conf=0.55), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("IsRepeatedGuest=0")))
#defined the rules to look for associations with rows in which people returned

inspectDT(rules1)

#For H1, NOT canceling is associated with:
#reservations with 2 or more adults (rule 4)
#reservations with no babies (rule 8)
#reservations with no children (rule 5)
#reservations with no babies and no children (rule 19)
#reservations with 2 or more adults and no children (rule 13)
#reservations with 2 or more adults, no children, and no babies (rule 26)
#we also find that not canceling is associated with not being a repeat guest (rule 7)
#not canceling is associated with getting the BB meal (rule 2)

inspectDT(rules2)

#For H1, NOT being a repeat guest is associated with:
#reservations with the "A" ReservedRoomType (rule 2)
#getting the BB meal (rule 4)
#being a "transient" customer type (rule 5)
#reservations with 2 or more adults (rule 6)
#reservations with no children (rule 7)
##reservations with no babies (rule 9)


################################   H2 ARM

levels(dataH2_new$IsRepeatedGuest)
#getting levels for factor variable

data2x <- as(dataH2_new, "transactions")
#coercing data2x into a transactions matrix

rules3 <- apriori(data2x, 
                  parameter=list(supp=0.5, conf=0.8), 
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("IsRepeatedGuest=0")))
#defined the rules to look for associations with rows in which people returned


inspectDT(rules3)

#For H2, NOT being a repeat guest is associated with:
#being a "transient" customer type (rule 3)
#getting the BB meal (rule 4)
#reservations with the "A" ReservedRoomType (rule 5)
#reservations with 2 or more adults (rule 6)
#reservations with no children (rule 7)
#reservations with no babies (rule 9)
#reservations with 2 or more adults and no children (rule 28)
#reservations with 2 or more adults and no babies (rule 30)
#reservations with no babies and no children (rule 32)


######################################################################  




###### Checking Correlation Matrix ######
cor_matrixH1 <- as.matrix(round((cor(Numerical_dataH1)[,"ADR"]),3))

colnames(cor_matrixH1) <- "ADR"

cor_matrixH1

# The correlation matrix shows us that after AVGrevperstay, Children is highly positive correlated to ADR and IsRepeatedGuest is inversely correlated
# to the ADR. 

cor_matrixH2 <- as.matrix(round((cor(Numerical_dataH2)[,"ADR"]),3))

colnames(cor_matrixH2) <- "ADR"

cor_matrixH2

# The correlation matrix shows us that after AVGrevperstay, Children and Adults are highly positive correlated to ADR and IsRepeatedGuest is highly inversely correlated
# to the ADR.    

##### Creating a Linear Models #####
dataH1.1$IsCanceled <- as.character(dataH1.1$IsCanceled)

dataH1.1$IsRepeatedGuest <- as.character(dataH1.1$IsRepeatedGuest)

dataH2.1$IsCanceled <- as.character(dataH2.1$IsCanceled)

dataH2.1$IsRepeatedGuest <- as.character(dataH2.1$IsRepeatedGuest)


# saving the final data to cleaned dataset for H1
data_cleanedH1 <- dataH1.1

# saving the final data to cleaned dataset for H2
data_cleanedH2 <-  dataH2.1



##### for H1- Resort Data #####

lmH1out <- lm(ADR~IsCanceled+LeadTime+StaysInWeekendNights+StaysInWeekNights+Adults+Children+Babies+IsRepeatedGuest+Agent+RequiredCarParkingSpaces+TotalOfSpecialRequests+AVGrevperstay+
                Meal+MarketSegment+DistributionChannel+DepositType+CustomerType+visitor_type+season+roomtypechanged, data = data_cleanedH1)

summary(lmH1out)

summary(lmH1out$residuals)

# Checking for Multicolinearity # 

# install.packages("car")
library(car)

# using the vif function from car package to check the variance inflation factor in other words correlation between independent variables.
vif(lmH1out)

# As the values of (GVIF^(1/(2*Df)))^2 for lmH1out are less than 5, we can safely say that our model does not have 
# effect of multicolinearity. Now we can proceed to explain the regression model results.



# Explanation of linear regression model "lmH1out" for H1- Resort data #
# In the above experiment, if we look at the summary of the residuals, the mean is exactly at 0, 
# so our model is giving proper linear relationships between predictors and the dependent variable. 
# The Multiple R-squared =  0.7828 and Adjusted R-squared = 0.7826 represents the proportion of about 78.26%
# variation in ADR (about its mean) explained by the multiple linear regression model with predictors in the
# model. The global F -test results given by the model represents that, F(35,39549) = 4073 , P-value < 2.2e-16,
# the model is significant as the p-value is less than the threshold value of 0.001. Now if we look at the 
# coefficients(estimate) values of the predictors we can see that the ADR value is increasing if the 
# reservation is getting canceled or no-show [7.37 estimate]. The reservation status with higher stays 
# during week nights [-8.393 estimate] and weekend nights [-10.88 estimate] affects inversely to the ADR value. 
# Somehow if the customer is mostly repeated [-8.28 estimate] then it is affecting inversely to the ADR. 
# If customer is getting required car parking space [6.569 estimate] then it is affecting positively to the ADR. 
# The meal types FB [11.41 estimate] and HB [12.31 estimate] are contributing highly positively to ADR.
# In addition to this if the meal is undefined [18.38 estimate], it is also contributing highly positively to ADR.
# All the market segments are highly positively predicting the ADR. Furthermore undefined distribution channel 
# [-43.50 estimate] is affecting ADR highly inversely. The family [20.57 estimate] visitor type is affecting ADR 
# positively whereas single [-12.34 estimate] visitor type is affecting ADR inversely. Among seasons, 
# spring [-36.67 estimate], summer [-19.49 estimate] and winter [-37.58 estimate] are affecting inversely to the ADR.
# if the room type is not changed [6.84 estimate] then it is likely that the ADR value would increase. All of the above 
# mentioned coefficients are significant to the model. Among all of the predictors with their different categories we have 
# Babies, SC meal type, direct distribution channel, undefined distribution channel, refundable deposit, group and 
# transient-party customer types are not significant as their P-values are greater than the assumed alpha threshold 
# value of 0.05 

### Below plots show the effects of linear relationship with the ADR value ###

ggplot(data_cleanedH1, aes(StaysInWeekNights,ADR))+ geom_point(color = "red")

ggplot(data_cleanedH1, aes(StaysInWeekendNights,ADR))+ geom_point(color = "red")

# As we can see from the scatter plots above, with increase in the stay in week-nights and the weekend nights, the ADR value is decreasing gradually.


##### for H2- City Data #####

lmH2out <- lm(ADR~IsCanceled+LeadTime+StaysInWeekendNights+StaysInWeekNights+Adults+Children+Babies+IsRepeatedGuest+Agent+RequiredCarParkingSpaces+TotalOfSpecialRequests+AVGrevperstay+
                Meal+MarketSegment+DistributionChannel+DepositType+CustomerType+visitor_type+season+roomtypechanged, data = data_cleanedH2)

summary(lmH2out)

summary(lmH2out$residuals)

# Checking for Multicolinearity # 
library(car)

# using the vif function from car package to check the variance inflation factor in other words correlation between independent variables.
vif(lmH2out)

# As the values of GVIF^(1/(2*Df)) for lmH2out shows that Adults variable has 25.000180 value much greater than 5. 
# In other words, Adults variable is having high multicolinearity with other predictors. Thus we need adjust our model 
# to remove the effects of multicolinearity. Let us adjust the model by removig Adults variable.

### adjusting the model ###
lmH2out_new <- lm(ADR~IsCanceled+LeadTime+StaysInWeekendNights+StaysInWeekNights+Children+Babies+IsRepeatedGuest+Agent+RequiredCarParkingSpaces+TotalOfSpecialRequests+AVGrevperstay+
                    Meal+MarketSegment+DistributionChannel+DepositType+CustomerType+visitor_type+season+roomtypechanged, data = data_cleanedH2)

summary(lmH2out_new)

summary(lmH2out_new$residuals)

# Checking for Multicolinearity # 
library(car)

# using the vif function from car package to check the variance inflation factor in other words correlation between independent variables.
vif(lmH2out_new)

# As the values of (GVIF^(1/(2*Df)))^2 for lmH2out_new are less than 5, we can safely say that our model does not have 
# effect of multicolinearity. Now we can proceed to explain the regression model results.

# Explanation of linear regression model "lmH1out" for H2- City data #

# In the above experiment, if we look at the summary of the residuals, the mean is exactly at 0, 
# so our model is giving proper linear relationships between predictors and the dependent variable. 
# The Multiple R-squared =  0.7603 and Adjusted R-squared = 0.7602 represents the proportion of about 76%
# variation in ADR (about its mean) explained by the multiple linear regression model with predictors in the
# model. The global F -test results given by the model represents that, F(34,78884) = 7358 , P-value < 2.2e-16,
# the model is significant as the p-value is less than the threshold value of 0.001. Now if we look at the 
# coefficients (estimate) ADR value increases with canceled or no-show [1.66 estimate] reservations. The reservation 
# with higher stays during week nights [-19.97 estimate] and weekend nights [-20.63 estimate] affects inversely to the 
# ADR value. Children [13.04 estimate] variable is affecting highly positively to the ADR. This also reflects into 
# visitor type as family [15.05 estimate]. If the guest is repeated [-16.79] it affects inversely to the ADR. In this 
# dataset if customers get required car parking space then it will affect positively to increase ADR. Meal types 
# FB [6.5 estimate] and HB [8.51 estimate] affects positively to increase ADR. Whereas, meal type SC [-5.62 estimate] 
# affects inversely to the ADR. Among market segments, complementary [-69.48 estimate], Corporate [-11.72 estimate], 
# Groups [-9.744 estimate], offline TA/TO [-6.04 estimate] are affecting inversely to the ADR. Whereas, market segments, 
# direct [4.26 estimate] and online TA [4.93 estimate] are affecting positively to increase the ADR. Non refundable deposits
# [5.048 estimate] increases ADR positively and refundable deposits [-8.97 estimate] affects ADR inversely. The hotels ADR 
# will increase positively with customer types Group [6.23 estimate], Transient [5.21 estimate], Transient-Party [4.17 estimate]. 
# In addition to this, spring [-7 estimate] and Winter [-6.658 estimate] seasons affects ADR inversely. 
# All of the above mentioned coefficients are significant to the model. Now among all the predictors in the model babies is the 
# only variable not significant as its p value: 0.227773 > 0.05 the assumed alpha threshold value.


### Below plots show the effects of linear relationship with the ADR value ###
ggplot(data_cleanedH2, aes(StaysInWeekNights,ADR))+ geom_point(color = "red")

ggplot(data_cleanedH2, aes(StaysInWeekendNights,ADR))+ geom_point(color = "red")

# As we can see from the scatter plots above, with increase in the stay in week-nights and the weekend nights, the ADR value is decreasing gradually.


ggplot(data_cleanedH2, aes(IsRepeatedGuest, ADR, fill = as.factor(IsCanceled)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("if the guest is repeated") + labs(fill = "Is Canceled")
# If the guest is repeated then it is most likely that the ADR value will decrease.

ggplot(dataH2.1, aes(IsRepeatedGuest, ADR, fill = as.factor(ReservationStatus)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = 'Set2')+xlab("if the guest is repeated") + labs(fill = "Reservation Status")


######### Predicting whether the reservations will get canceled or not using Logistic regression modeling technique ####

### creating Logistic regression model for H1-Resort data ###


# saving cleaned data into dataH1logit for logistic regression model 
dataH1logit <- dataH1.1
dataH1logit$IsCanceled <- as.factor(dataH1.1$IsCanceled)


# using glm function to create generalized linear model to predict the IsCanceled variable: 1-Cancellation, 0-no cancellations
glmH1out <- glm(IsCanceled~ADR+LeadTime+StaysInWeekendNights+StaysInWeekNights+Children+Babies+IsRepeatedGuest+Agent+RequiredCarParkingSpaces+TotalOfSpecialRequests+AVGrevperstay+
     Meal+MarketSegment+DistributionChannel+DepositType+CustomerType+visitor_type+season+roomtypechanged, data = dataH1logit, family = binomial(link = "logit"))



### checking for Multicolinearity ###
library(car)

# using the vif function from car package to check the variance inflation factor in other words correlation between independent variables.
vif(glmH1out)

# As the values of vif for glmH1out are less than 10, we can safely say that our model does not have effect of 
# multicolinearity. Now we can proceed to explain the regression model results.

## exploring the model results for H1- Resort ##
summary(glmH1out)

## since all the coefficients are the log-odds we need to convert it to normal odds by taking their exponent values to understand their significance to the model
exp(coef(glmH1out))[order(-exp(coef(glmH1out)))]


# In the above experiment, we can see that the coefficients have reduced the Null deviance from 46931 to Residual deviance of 31382, 
# which suggests that the model is predicting the canceled reservations by reducing the residual errors. The normal odds are predicting 
# whether the reservation is likely to get canceled i.e, 1.
# There are many significant predictors in the model. Let us understand the major effects due to predictors with highest coefficients 
# to the Iscanceled variable. For each one unit change in Non Refundable DepositType will increase the normal odds 31.43 of getting 
# the registraion canceled. Also, for each one unit change in room type not changed data will increase the normal odds 6.96 of 
# getting the registraion canceled. Furthermore, one unit change in Transient customer type will increase the normal odds of 3.1 for 
# the registration getting canceled. Additionally all the one unit change in the seasons Spring [4.043 estimate], Winter [2.5 estimate]
# and Summer [2 estimate] increase the reservation's cancellation by the amount of coefficients. 






### creating Logistic regression model for H2-City data ###
dataH2logit <- dataH2.1
dataH2logit$IsCanceled <- as.factor(dataH2.1$IsCanceled)


# using glm function to create generalized linear model to predict the IsCanceled variable: 1-Cancellation, 0-no cancellations
glmH2out <- glm(IsCanceled~ADR+LeadTime+StaysInWeekendNights+StaysInWeekNights+Children+Babies+IsRepeatedGuest+Agent+RequiredCarParkingSpaces+TotalOfSpecialRequests+
                  Meal+MarketSegment+DistributionChannel+DepositType+CustomerType+visitor_type+season+roomtypechanged, data = dataH2logit, family = binomial(link = "logit"))



### checking for Multicolinearity ###

# using the vif function from car package to check the variance inflation factor in other words correlation between independent variables.
vif(glmH2out)

## exploring the model results for H2- City ##
summary(glmH2out)

## since all the coefficients are the log-odds we need to convert it to normal odds by taking their exponent values to understand their significance to the model
round(exp(coef(glmH2out))[order(-exp(coef(glmH2out)))],2)


# In the above experiment, we can see that the coefficients have reduced the Null deviance from 107265 to Residual deviance of 68013, 
# which suggests that the model is predicting the canceled reservations by reducing the residual errors. The normal odds are predicting whether the reservation is likely to get canceled i.e, 1.
# There are many significant predictors in the model. Let us understand the major effects due to predictors with highest coefficients 
# to the Iscanceled variable. For each one unit change in Non Refundable DepositType will increase the normal odds 687.58 of getting 
# the registration canceled. Also, for each one unit change in meal type FB will increase the normal odds 20.82 of getting the 
# registration canceled. Additionally, every one unit chnage in DepositType Refundable will increase the normal odds 7.57 of getting 
# registration canceled. If the room type is not changed then there is also increase in the normal odds 6.58 of getting registration 
# canceled. Market segments, complementary [3.13 estimate] and online TA [2.67 estimate] are predicting that the registrations may 
# get canceled. Finally for every one unit change in the spring season there is normal odds of 2.26 that the reservation may get 
# canceled.  



###### Creating SVM Model #####



### For H1- Resort data ###

#install.packages("kernlab")
#install.packages("caret")

library(kernlab)
library(caret)

dataH1SVM <- dataH1.1


dataH1SVM$IsCanceled <- as.factor(dataH1SVM$IsCanceled)

dataH1SVM <- subset(dataH1SVM , select = c("IsCanceled","LeadTime","StaysInWeekendNights","StaysInWeekNights","Adults","Children","Babies","Agent","ADR","RequiredCarParkingSpaces","TotalOfSpecialRequests","AVGrevperstay"))

trainList_IsCanceledH1 <- createDataPartition(y=dataH1SVM$IsCanceled,p=.60,list=FALSE)

# creating train data into trainSet_IsCanceledH1
trainSet_IsCanceledH1 <- dataH1SVM[trainList_IsCanceledH1,]

# creating test data into testSet_IsCanceledH1
testSet_IsCanceledH1 <- dataH1SVM[-trainList_IsCanceledH1,]

# using dim() to get the dimensions
dim(trainSet_IsCanceledH1)

# using dim() to get the dimensions
dim(testSet_IsCanceledH1)

# colnames(Numerical_dataH1)

svmout_IsCanceledH1 <- ksvm(IsCanceled~., data = trainSet_IsCanceledH1, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)

svmout_IsCanceledH1

# In the above experiment, of ksvm model the Training error : 0.215477 or 21.547% and Cross validation error : 0.232444 or 23.2444%
# The cross-validation error is higher, because a set of parameters never performs as well on subsequent data sets as
# it does with the original training set. This looks like a good model generated using  

# The C argument refers to the so-called cost of constraints. The places where we get opposite results for opposite categories, 
# we can reduce those using lower value of C. If we will use higher value of C we might get fewer mistakes but only at the cost 
# of having very close margin between two different points from opposite categories and it may create a weird angle between these 
# two categories. On the other hand, if we have a low value of C we will get a generalizable model, but one that makes more 
# classification mistakes.
# 
# In the argument cross=3, cross refers to the cross-validation model that the algorithm uses. In this case, our choice of the final 
# parameter, prob.model=TRUE, dictates that we use a so-called threefold cross-validation in order to generate the probabilities 
# associated with whether a credit risk is high or low. Cross- validation is important for avoiding the problem of overfitting.
# 
# By using k-fold (in this case threefold) cross-validation, we can rein in the fitting function so that it does not work so hard 
# and so that it does create a model that is more likely to generalize to other data.

# using predict function to predict the Iscanceled variable for testSet_IsCanceledH1 data using svmout_IsCanceledH1 model.
svm_pred_IsCanceledH1 <- predict(svmout_IsCanceledH1,testSet_IsCanceledH1, type = "response")

summary(svm_pred_IsCanceledH1)

# the svm_pred_IsCanceledH1 model predicted 13602 values for reservations likely not to get canceled i.e. 0 and 2231 values for reservations likely to get canceled i.e. 1 for testSe dataset.

# Creating confusionMatrix using Caret Package
cnf_mtrxH1<- confusionMatrix(data = as.factor(svm_pred_IsCanceledH1), reference = testSet_IsCanceledH1$IsCanceled)

cnf_mtrxH1

# Calculating the error rate of the model
# Error Rate = 1 - Accuracy
error_rateH1 <- 1 - 0.7723

error_rateH1

# The confusion matrix shows that the model is predicting 10700 values for reservations likely not to get canceled i.e. 0 
# and 1528 values for reservations likely to get canceled i.e. 1 correctly for test dataset. Whereas, the matrix shows, 
# model has predicted 1 for 703 values incorrectly and predicted 0 for 2902 values incorrectly for test dataset. 
# Also the error rate is 22.77%  and the model has 77.23% accuracy to predict values correctly.  





### For H2- City data ###


library(kernlab)
library(caret)

dataH2SVM <- dataH2.1


dataH2SVM$IsCanceled <- as.factor(dataH2SVM$IsCanceled)

dataH2SVM <- subset(dataH2SVM , select = c("IsCanceled","LeadTime","StaysInWeekendNights","StaysInWeekNights","Adults","Children","Babies","Agent","ADR","RequiredCarParkingSpaces","TotalOfSpecialRequests","AVGrevperstay"))

# creating indices and storing them in trainList_IsCanceledH2 by using createDataPartition function, which will do a stratified sampling ratio 
# of 1 canceled to 0 not canceled from IsCanceled variable(dependent variable) the ratio stays the same for actual, 
# train and test data by using p=.60(picking 60% indices)

trainList_IsCanceledH2 <- createDataPartition(y=dataH2SVM$IsCanceled,p=.60,list=FALSE)

# creating train data into trainSet_IsCanceledH2
trainSet_IsCanceledH2 <- dataH2SVM[trainList_IsCanceledH2,]

# creating test data into testSet_IsCanceledH2
testSet_IsCanceledH2 <- dataH2SVM[-trainList_IsCanceledH2,]

# using dim() to get the dimensions
dim(trainSet_IsCanceledH2)

# using dim() to get the dimensions
dim(testSet_IsCanceledH2)

# Building SVM based on train data using ksvm function
svmout_IsCanceledH2 <- ksvm(IsCanceled~., data = trainSet_IsCanceledH2, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)

svmout_IsCanceledH2

# In the above experiment, of ksvm model the Training error : 0.240328 or 24.033% and Cross validation error : 0.25 or 25.00%
# The cross-validation error is higher, because a set of parameters never performs as well on subsequent data sets as
# it does with the original training set. This looks like a good model.

# The C argument refers to the so-called cost of constraints. The places where we get opposite results for opposite categories, 
# we can reduce those using lower value of C. If we will use higher value of C we might get fewer mistakes but only at the cost 
# of having very close margin between two different points from opposite categories and it may create a weird angle between these 
# two categories. On the other hand, if we have a low value of C we will get a generalizable model, but one that makes more 
# classification mistakes.
# 
# In the argument cross=3, cross refers to the cross-validation model that the algorithm uses. In this case, our choice of the final 
# parameter, prob.model=TRUE, dictates that we use a so-called threefold cross-validation in order to generate the probabilities 
# associated with whether a credit risk is high or low. Cross- validation is important for avoiding the problem of overfitting.
# 
# By using k-fold (in this case threefold) cross-validation, we can rein in the fitting function so that it does not work so hard 
# and so that it does create a model that is more likely to generalize to other data.


# using predict function to predict the Iscanceled variable for testSet_IsCanceledH2 data using svmout_IsCanceledH2 model.
svm_pred_IsCanceledH2 <- predict(svmout_IsCanceledH2,testSet_IsCanceledH2, type = "response")

summary(svm_pred_IsCanceledH2)

# the svm_pred_IsCanceledH2 model predicted 20129 values for reservations likely not to get canceled i.e. 0 and 11438 values for reservations likely to get canceled i.e. 1 for testSet dataset.

# Creating confusion Matrix using Caret Package
cnf_mtrxH2<- confusionMatrix(data = as.factor(svm_pred_IsCanceledH2), reference = testSet_IsCanceledH2$IsCanceled)

cnf_mtrxH2

# Calculating the error rate of the model
# Error Rate = 1 - Accuracy

error_rateH2 <- 1 - 0.7531

error_rateH2

# The confusion matrix shows that the model is predicting 15356 values for reservations likely not to get canceled i.e. 0 
# and 4773 values for reservations likely to get canceled i.e. 1 correctly for test dataset. Whereas, the matrix shows, 
# model has predicted 1 for 3021 values incorrectly and predicted 0 for 8417 values incorrectly for test dataset. 
# Also the error rate is 24.69%  and the model has 75.31% accuracy to predict values correctly. 





#### The effects of average revenue per stay on whether the reservation would get canceled or not ####
ggplot(dataH1.1, aes(IsCanceled,AVGrevperstay)) + geom_boxplot() + ggtitle("For H1-Resort Data")

ggplot(dataH2.1, aes(IsCanceled,AVGrevperstay)) + geom_boxplot() + ggtitle("For H2-City Data")

# As we can see from the box-plots above for both H1-Resort and H2-City data, the average revenue per stay shows no clear distinction between reservation is canceled or not. This suggests that average revenue per stay is not a great with the available data.




#### The effects of ADR on whether the reservation would get canceled or not ####
ggplot(dataH1.1, aes(IsCanceled,ADR)) + geom_boxplot() + ggtitle("For H1-Resort Data")

ggplot(dataH2.1, aes(IsCanceled,ADR)) + geom_boxplot() + ggtitle("For H2-City Data")

# As we can see from the box-plots above for both H1-Resort and H2-City data, the average daily rate shows no clear distinction between reservation is canceled or not. This suggests that average revenue per stay is not a great with the available data.

