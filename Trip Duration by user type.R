setwd("/home/john86/Divvy/Divvy_Stations_Trips_2013") # set working directory
all_dat <- read.csv("all_dat.csv") #read in previously created file, rather re-create every time


all_dat$trip_length <- as.numeric(gsub(",","",all_dat$tripduration))/60

all_dat$trip_category[all_dat$trip_length >=   0 & all_dat$trip_length < 30] <- '1'
all_dat$trip_category[all_dat$trip_length >=  30 & all_dat$trip_length < 60] <- '2'
all_dat$trip_category[all_dat$trip_length >=  60 & all_dat$trip_length < 90] <- '3'
all_dat$trip_category[all_dat$trip_length >=  90] <- '4'



sub_prop <- table(all_dat$usertype,all_dat$trip_category)[c(2,4,6,8)]/sum(table(all_dat$usertype,all_dat$trip_category)[c(2,4,6,8)])

cus_prop <- table(all_dat$usertype,all_dat$trip_category)[c(1,3,5,7)]/sum(table(all_dat$usertype,all_dat$trip_category)[c(1,3,5,7)])

prop_df <- data.frame(c(cus_prop,sub_prop),c(0.6,1.6,2.6,3.6,1,2,3,4),c(rep("Customer",4),rep("Subscriber",4)) )
colnames(prop_df) <- c("Props","bin","User")




require(ggplot2)
rec1 <- ggplot(prop_df, aes(xmin = bin,xmax=bin+0.4,ymin=0,ymax=Props*100,fill=User)) + geom_rect()
rec1 + scale_y_continuous(name="Percentage of Rides") +
       scale_x_discrete(name="Trip Duration in Minutes",labels = c("","0 to 30 \n Free \n Free " ,"30 to 60","60 to 90","90 +"))


rec1 + scale_y_continuous(name="Percentage of Rides") +
  scale_x_continuous(name="Trip Duration in Minutes",labels = c("0 to 30" ,"30 to 60","60 to 90","90 +"),breaks = c(1,2,3,4)) +
  theme(legend.position=c(0.75,0.75)) +
  geom_text(aes(1.2,105,label = "No additional cost", color = "Customer"),show_guide = FALSE) +
  geom_text(aes(1.2,101,label = "No additional cost", color = "Subscriber"),show_guide = FALSE) +
  geom_text(aes(2,27,label ="+ $2.00",color = "Customer"),show_guide = FALSE) +
  geom_text(aes(2,22,label ="+ $1.50",color = "Subscriber"),show_guide = FALSE) +
  geom_text(aes(3,17,label ="+ $6.00",color = "Customer"),show_guide = FALSE) +
  geom_text(aes(3,12,label ="+ $4.50",color = "Subscriber"),show_guide = FALSE) +
  geom_text(aes(4,12,label ="+ $8.00",color = "Customer"),show_guide = FALSE) +
  geom_text(aes(4,7,label ="+ $6.00",color = "Subscriber"),show_guide = FALSE) 


