
#Reading the uber data file into 'uber' df
uber <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE)

#Viewing the dataset
View(uber)
#Upon viewing the dataset, there are multiple formats for Request and drop timestamps
# Let's see the structure of the dataframe
str(uber)
# The request and drop timestamps are in character format, we need to convert them in to R standard date time formt.
typeof(uber$Request.timestamp)
typeof(uber$Drop.timestamp)

# We will user lubricate function to clean the date format and convert it to the standard format.
library(lubridate)

uber$Request.timestamp <- parse_date_time(uber$Request.timestamp,orders = c("d/m/Y H:M","d-m-Y H:M:S"))
typeof(uber$Request.timestamp)
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp,orders = c("d/m/Y H:M","d-m-Y H:M:S"))
typeof(uber$Drop.timestamp)
str(uber)
# These are now in standard date time format.

#Lets derive new useful metrics from the datetime columns.
uber$Request.sec <- format(uber$Request.timestamp, "%S")  # Deriving Seconds from the reuqest timestamp
uber$Request.min <- format(uber$Request.timestamp, "%M")  # Deriving Minutes from the reuqest timestamp
uber$Request.hour <- format(uber$Request.timestamp, "%H") # Deriving Hours from the reuqest timestamp
uber$Request.date <- as.Date(uber$Request.timestamp)      # Deriving Date from the reuqest timestamp
uber$Request.day <- format(uber$Request.date, "%d")       # Deriving Day from the reuqest timestamp
uber$Request.month <- format(uber$Request.date, "%m")     # Deriving Month from the reuqest timestamp

uber$Drop.sec <- format(uber$Drop.timestamp, "%S")        # Deriving Seconds from the drop timestamp
uber$Drop.min <- format(uber$Drop.timestamp, "%M")        # Deriving Minutes from the drop timestamp
uber$Drop.hour <- format(uber$Drop.timestamp, "%H")       # Deriving Hours from the drop timestamp
uber$Drop.date <- as.Date(uber$Drop.timestamp)            # Deriving Date from the drop timestamp
uber$Drop.day <- format(uber$Drop.date, "%d")             # Deriving Day from the drop timestamp
uber$Drop.month <- format(uber$Drop.date, "%m")           # Deriving Month from the drop timestamp

summary(uber)

#Conveting the categorical variables to factors for better insights.
uber$Status <- factor(uber$Status)
uber$Pickup.point<-factor(uber$Pickup.point)

summary(uber)

#Now that we have data in proper readable format, lets start some analysis.
#### Uninvariate and Segmented analysis! ####

unique(uber$Request.month) # 07 It has only a single month, proper insights cant be drawn from this.
unique(uber$Drop.month)   # 07 It has only a single month, proper insights cant be drawn from this.

library(ggplot2)
ggplot(uber,aes(Request.day)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1) 
#The aesthetics here is the day when the request is made, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value. 
# This is the widespread of requests across 5 days. And its almost constant.
#Let us not consider this as well and the Drop.day too, as it is dependent on the requests made.

unique(uber$Pickup.point) # The travel is from Airport-city and city-Airport.
ggplot(uber,aes(Pickup.point)) + geom_bar() +geom_text(stat='count',aes(label=..count..),vjust=-1)
#The aesthetics here is pickup point where the request is done. 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#Clearly there are more requests from city but the fraction is too small.

unique(uber$Status)     # Cancelled Trip Completed No Cars Available
ggplot(uber,aes(Status)) + geom_bar() +geom_text(stat='count',aes(label=..count..),vjust=-1)
#The aesthetics here is the status of the request initialized by the rider, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#Successful trip completions are more in number but not if cancellations and no cars availabitily combined together.
#Lets us dig more on this and see the problems behind this.

ggplot(uber, aes(factor(Request.hour))) + geom_bar(stat="count",col="red")  +labs(x="Hour",y="Total requests")+ geom_text(stat='count',aes(label=..count..),vjust=-1)
#The aesthetics here is the hour when the requests are made during the whole period, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#Label names are given to each of the axes and red color is given for the outline of the bars.
#We can see patterns around here across various timings of the day.
#Let us group these into various time slots and find interesting insights out of it.
uber$Request.hour<-as.numeric(uber$Request.hour)

uber$slot <- ifelse(uber$Request.hour>=0 & uber$Request.hour<4,"Midnight",
              ifelse(uber$Request.hour>=4 & uber$Request.hour<8,"EarlyMorning",
                 ifelse(uber$Request.hour>=8 & uber$Request.hour<12,"Morning",
                    ifelse(uber$Request.hour>=12 & uber$Request.hour<16,"Noon",
                       ifelse(uber$Request.hour>=16 & uber$Request.hour<20,"Evening",
                          ifelse(uber$Request.hour>=20 & uber$Request.hour<24,"Night","nil"))))))

#We have now grouped the timings into various slots arounnd the day. Lets plot the graph.
ggplot(uber,aes(slot)) +geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)
#The aesthetics here is the slot which is derived from the hour when the request was made, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
# We can see there are more number of requests in the Evening's and Early Morning.

#Let us see the drop hours
ggplot(uber, aes(factor(Drop.hour))) + geom_bar()
#There are NA values in this, lets see if this is impacting our analysis.
unique(uber[which(is.na(uber$Drop.hour)),4 ])
#There are NA values because the status of the cabs were not available and are cancelled. This makes sense.
#This will not impact our analysis.

# Let us check the requests based on pickup point during different time slots.
ggplot(uber,aes(slot))+geom_bar()+facet_wrap(~uber$Pickup.point)+
ggtitle("Total requests during different Time Slots")+
  labs(x="Time Slots",y="Total requests")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)
#The aesthetics here is the slot which is derived from the hour when the request was made, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#We are printing the title using ggtitle and printing the axes labels
#It is now clear that more no of req are during evening from Airport and Early morning from City.

#1 Visually identify the most pressing problems for Uber
# Let us check and spot why there are no availality of cars and cancellations of bookings.
#Subsetting the no availality of cars and cancellations of bookings information into 'uber_noshow; df.
uber_noshow <- subset(uber,uber$Status =="No Cars Available" | uber$Status=="Cancelled")
#Let us plot the status during different time slots and pickup points.
ggplot(uber_noshow,aes(slot,fill=Status)) + geom_bar(position = "stack") + facet_wrap(~uber_noshow$Pickup.point)
#The aesthetics here is the slot which is derived from the hour when the request was made from the uber_noshow df, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#facet wrap is used on pickup point to bring the facets together
#Now, we came to know the problems 
# a. there are no cars available at the airport in the evening;
# b. And, booking cancellations are more in city specially in the early morning hours. 

#2.Find out the gap between supply and demand and show the same using plots.
# Demand - Is the total no of requests.
# Supply - is the succesfull trip completions.
# Let us take the Trip completed in new DF.
uber_trip_complete<- subset(uber,uber$Status=="Trip Completed")

#Plotting the demand plot and saving into dataset.
plot1 <- ggplot(uber,aes(slot))+geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1)+labs(x="Demand")
#The aesthetics here is the slot which is derived from the hour when the request was made, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#PLotting the supply and saving in to an other dataset.
plot2<-ggplot(uber_trip_complete,aes(slot))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)+labs(x="Supply")
#The aesthetics here is the slot which is derived from the hour when the request was made from the uber_trip_complete df, 
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
library(gridExtra)
#Aranging the plots side by side to differentiate the gap.
grid.arrange(plot1,plot2,ncol=2)
#The plots easily show the variation between Demand and Supply, on observingthe scale on y axis.
#Let us find the highest gap among the timeslots.
#Calculating the Demand-Supply to find the gap.

length(which(uber$slot=="EarlyMorning")) - length(which(uber_trip_complete$slot=="EarlyMorning"))#848
length(which(uber$slot=="Evening")) - length(which(uber_trip_complete$slot=="Evening"))#988
length(which(uber$slot=="Midnight")) - length(which(uber_trip_complete$slot=="Midnight"))#239
length(which(uber$slot=="Morning")) - length(which(uber_trip_complete$slot=="Morning"))#709
length(which(uber$slot=="Night")) - length(which(uber_trip_complete$slot=="Night"))#879
length(which(uber$slot=="Noon")) - length(which(uber_trip_complete$slot=="Noon"))#251

#We can see the hightest gap is in the evening's
#Let us subset the evening records and find out type of request the gap is for.

uber_Evening <-subset(uber,uber$slot=="Evening")
#Plotting the graph for evening Demand and supply gap on basis of pickup point. 
ggplot(uber_Evening,aes(Status)) + geom_bar(col="yellow")+facet_wrap(~uber_Evening$Pickup.point)
#The aesthetics here is the status from the evening slot of the dataset.
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#Plot clearly shows the pickup point at airport has more gap in supply and demand.
3.#And the issue is there are no cars available per the above plot
#To figure out this , lets plot another grpah to undestand the situation better. 
ggplot(uber,aes(slot,fill=Status))+geom_bar()+facet_wrap(~uber$Pickup.point)+
  ggtitle("Total requests during different Time Slots")+
  labs(x="Time Slots",y="Total requests")
#The aesthetics here is the slot which is derived from the hour when the request was made and the bar
#fill is arranged based on status.
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#Title and axes names are printed using ggtitle and labs.
#Here ,we can see the total no of cabs coming in to the airport from city are verly less in the evening's,
#And, the requests are more at this time from airport, thus there are no cabs available at the airport.

#We have also identified cancellations as another major problem.
#In the same plot above, we can see that the cancellations are more in the early morning hours from city.
#Let us observe this closely using the below plot. 
uber_EarlyMorning <-subset(uber,uber$slot=="EarlyMorning")
ggplot(uber_EarlyMorning,aes(Status))+geom_bar()+facet_wrap(~uber_EarlyMorning$Pickup.point)
#The aesthetics here is the status of the reqests made from the early morning slot.
#bar graph is used to represent the count of cases in each category with a discrete set of values.
#geom_text is used to print the count on each bar with an display adjustment value.
#Looks like the no of requests from airport is half the number compared to city.
#So the drivers are cancelling their orders. There are more requests in the evening but they have to wait
#till evening inturn losing their fares becaue of idle time.

#4.Recommend some ways to resolve the supply-demand gap.
# A.Fare to the airports can be increased so that the profit margin is more in compensation to the idle time.
# B.Dedicated cabs can be introduced to and from airport/city.

