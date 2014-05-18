require(ggplot2)
require(sqldf)
setwd("/home/john86/Divvy/Divvy_Stations_Trips_2013") # set working directory
all_dat <- read.csv("all_dat.csv") #read in previously created file, rather re-create every time


all_dat$trip_length <- as.numeric(gsub(",","",all_dat$tripduration))/60

step1 <- sqldf("select usertype, start_hour,start_wkday_wkend, count(start_hour) as total_rides
  from all_dat
  group by usertype, start_hour, start_wkday_wkend")

dates <- sqldf("select distinct just_start_date, start_wkday_wkend
               from all_dat")
dates2 <- sqldf("select start_wkday_wkend, count(start_wkday_wkend) as total_days
                from dates
                group by start_wkday_wkend")
averages <- sqldf("select *
                  from step1, dates2
                  where step1.start_wkday_wkend = dates2.start_wkday_wkend")

averages2 <-sqldf("select usertype, start_hour, start_wkday_wkend, total_rides/total_days as average_day
                  from averages")

ggplot(data = averages2,aes(x=start_hour,y=average_day,fill=usertype)) + 
  geom_bar(stat='identity',width = 1) +
  facet_grid(start_wkday_wkend~usertype) +
  coord_flip() +
  scale_x_continuous(name = "Departure Hour", breaks=c(0:23),labels=gsub(" ","",paste(c(0:23),":00"))) +
  scale_y_continuous(name = "Average Rides in a Day") +
  theme(legend.position="none")
  



averages
sample_obs <- sample(976588,20000,replace = FALSE)

all_dat_sample <- all_dat[c(sample_obs),]


ggplot(data = all_dat,aes(x=start_hour,fill=usertype)) + 
  geom_histogram(binwidth=1) + 
  facet_grid(start_wkday_wkend~usertype) + 
  coord_flip() +
  scale_x_continuous(name = "Departure Hour", breaks=c(0:23),labels=gsub(" ","",paste(c(0:23),":00"))) +
  scale_y_continuous(breaks = c(10000,20000,30000,40000,50000),labels=c("10","20","30","40","50"),name="Total Rides (in 1000's)") +
  theme(legend.position="none")
