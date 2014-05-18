setwd("/home/john86/Divvy/Divvy_Stations_Trips_2013")
ride_dat <- read.csv("Divvy_Trips_2013.csv")
station_dat <- read.csv("Divvy_Stations_2013.csv")

library(ggmap)
library(sqldf)
library(stringr)
library(scales)


ride_dat$route = paste(ride_dat$to_station_name,"|",ride_dat$from_station_name)


all_dat_from <- sqldf("select station_dat.*,ride_dat.* from station_dat, ride_dat
                 where station_dat.name = ride_dat.from_station_name")
all_dat_to   <- sqldf("select station_dat.*,ride_dat.* from station_dat, ride_dat
                 where station_dat.name = ride_dat.to_station_name")

all_dat_from$from_longitude  <- all_dat_from$longitude
all_dat_from$from_latitude   <- all_dat_from$latitude
all_dat_from$from_dpcapacity <- all_dat_from$dpcapacity

all_dat_to$to_longitude      <- all_dat_to$longitude
all_dat_to$to_latitude       <- all_dat_to$longitude
all_dat_to$to_dpcapacity     <- all_dat_to$dpcapacity

all_dat      <- sqldf("select all_dat_to.*,all_dat_from.from_longitude, all_dat_from.from_latitude,all_dat_from.from_dpcapacity 
                        from all_dat_from, all_dat_to
                        where all_dat_from.trip_id = all_dat_to.trip_id")




# Create day of the week and time functions
all_dat$start_time <- strptime(as.character(all_dat$starttime),"%m/%d/%Y %H:%M")
all_dat$stop_time  <- strptime(as.character(all_dat$stoptime), "%m/%d/%Y %H:%M")
all_dat$just_start_time  <- format(all_dat$start_time,"%H:%M")
all_dat$just_stop_time   <- format(all_dat$stop_time,"%H:%M")
all_dat$just_start_date  <- format(all_dat$start_time,"%m/%d/%Y")
all_dat$just_start_date2 <- format(all_dat$start_time,"%Y/%m/%d")
all_dat$just_stop_date   <- format(all_dat$stop_time,"%m/%d/%Y")
all_dat$start_weekday    <- format(all_dat$start_time, '%A')
all_dat$stop_weekday     <- format(all_dat$stop_time, "%A")
all_dat$start_hour       <- format(all_dat$start_time, "%H")
all_dat$stop_hour        <- format(all_dat$start_time, "%H")
all_dat$month            <- format(all_dat$start_time, '%B')
all_dat$week             <- format(all_dat$start_time, '%W')







all_dat <- within(all_dat, start_weekday <- factor(start_weekday, 
                                         levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')))

# Remove unneeded columns from data frame
all_dat$name <- NULL
all_dat$longitude <- NULL
all_dat$dpcapacity <- NULL
all_dat$latitude <- NULL












# This dataset is too large to work with quickly. Lets take a sample of 10,000
# observations and play with that

obs <-  sample(1:dim(all_dat)[[1]],10000,replace=FALSE)


map <- get_map(location=c(lon=mean(all_dat$from_longitude),lat=mean(all_dat$from_latitude)), source="google",zoom=14)




sample <- sample(1:750000,20000,replace = FALSE)


sample_dat  <- all_dat[c(sample),]

mean(all_dat$from_longitude)
mean(all_dat$from_latitude)


map2 <- get_map(location=c(lon=-87.6625,lat=41.89), source="google",zoom=13)






all_dat$week   <- format(all_dat$just_start_date, format = '%W')


h3 <- ggplot(data=all_dat, aes(x=just_start_date2,fill=usertype)) + geom_histogram(stat='bin',position='dodge',breaks=c(25:52))
h2 + theme_bw() + coord_flip()
h3 + scale_x_date(labels=date_format("%B"), breaks = date_breaks("week"))
h3 + theme_bw()
require(ggplot2)
require(scales)
#play with stat_bin
b1 <- ggplot(data=all_dat, aes(x=just_start_date,fill=usertype))
b1 + geom_histogram(binwidth=7*24*60*60,position='dodge')

h4 <- ggplot(data = all_dat, aes(x=month,fill=usertype)) + geom_histogram(position='dodge')
h4


all_dat_sample <- all_dat[sample(1:750000,1000,replace = FALSE),]

all_dat_sample$week
all_dat_sample$User = all_dat_sample$usertype
all_dat$User = all_dat$usertype

p <- ggplot(all_dat,aes(x=as.Date(just_start_date2), fill=User))  

png("growth.png")
p +  geom_histogram(position='dodge',binwidth = 7) +
    scale_x_date(labels=date_format("%b"),name = "Date") +
    scale_y_continuous(name="Weekly Total Rides") +
    theme(legend.position = c(0.85,0.85))
dev.off()
  

