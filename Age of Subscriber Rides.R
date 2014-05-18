library(ggplot2)

# Need to have the dataset "all_dat" loaded already. 
subscribers <- subset(all_dat,usertype=="Subscriber")
subscribers$age <- 2013 - subscribers$birthyear
qs <- c(0.005,0.05,0.95,0.995)

quantile(subscribers$age,qs,na.rm=TRUE)
table(subscribers$gender)/403036

ggplot() + geom_histogram(data =subscribers,aes(x = age),binwidth=1) + theme_bw() +
  scale_x_continuous(name = "2013 Age of Rider") +
  scale_y_continuous(name = "Total Rides")
