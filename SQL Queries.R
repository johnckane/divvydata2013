require(sqldf)

sqldf("select route, count(route) as summary, median(tripduration) as median, avg(tripduration) as length 
      from routes_dat 
      where usertype = 'Subscriber'
      group by route
      order by 2 desc
      limit 450")

sqldf("select usertype, month, count(*) as month_count
      from oneway
      group by usertype, month ")
sqldf("select usertype, weekday, count(weekday), percentile_cont(0.05) within trip_lengths as _5th, percentile_cont(0.95) within trip_lengths as _95th
      from all_dat_both
      group by all_dat_both
      where trip = 'To'
      order by 1 desc
      ")

sqldf("select usertype, weekday, count(weekday) as N, count(weekday)/count(*) as pct, avg(tripduration) as length
      from all_dat_both 
      group by usertype, weekday
      order by 1 desc
      limit 450")

sqldf("select bikeid, count(bikeid)
      from ride_dat
      group by bikeid
      order by 2 desc
      limit ")

gain <- sqldf("select to_station_id, to_station_name, count(to_station_id) as gains,start_wkday_wkend
                from all_dat
                group by to_station_id,start_wkday_wkend")
loss <- sqldf("select from_station_id, from_station_name, count(from_station_id) as losses,start_wkday_wkend
                from all_dat
                group by from_station_id,start_wkday_wkend")

both <- sqldf("select gain.*, loss.*, gain.gains- loss.losses as net, gains+losses as total
                from gain, loss
                where ((gain.to_station_id = loss.from_station_id) and
                  (gain.start_wkday_wkend = loss.start_wkday_wkend))
                order by total")
both2 <- sqldf("select a.*, b.latitude, b.longitude
               from both as a, station_dat as b
               where a.to_station_name = b.name")

both2$pct <- both2$net/both2$total  

### Look by user type
gain_user <- sqldf("select to_station_id, to_station_name, count(to_station_id) as gains,usertype
                from all_dat
                group by to_station_id,usertype")
loss_user <- sqldf("select from_station_id, from_station_name, count(from_station_id) as losses,usertype
                from all_dat
                group by from_station_id,usertype")

both_user <- sqldf("select gain_user.*, loss_user.*, gain_user.gains- loss_user.losses as net, gains+losses as total
                from gain_user, loss_user
                where ((gain_user.to_station_id = loss_user.from_station_id) and
                  (gain_user.usertype = loss_user.usertype))
                order by total")
both2_user <- sqldf("select a.*, b.latitude, b.longitude
               from both_user as a, station_dat as b
               where a.to_station_name = b.name")

both2_user$pct <- both2_user$net/both2_user$total  


ggplot(both2,aes(x=longitude,y=latitude)) +
  stat_density2d(aes(fill=..level.., alpha=0.1),
                 size = 3, bins = 5, geom="polygon") + facet_grid(.~start_wkday_wkend)

ggplot(both2,aes(x=longitude, y = latitude,colour=net)) +
  geom_point() + 
  scale_colour_gradient(low='gray', high = 'black') +
  facet_grid(.~start_wkday_wkend)


ggplot(both2,aes(x=longitude, y = latitude,colour=pct,alpha = 0.1)) +
  geom_point(aes(size=log(total))) + 
  scale_colour_gradient(low='red', high = 'blue') +
  facet_grid(.~start_wkday_wkend)
require(ggmap)
?get_map

my_map   = get_map(location = c(-87.8, 41.74, -87.58, 42), maptype = 'roadmap')
my_map2  = get_map(location = c(-87.75, 41.77, -87.55, 42), maptype = 'roadmap')
my_map3  = get_map(location = c(-87.725,41.88,-87.55,42),maptype = 'roadmap')
my_map4  = get_map(location = c(-87.725,41.76,-87.55,41.88),maptype = 'roadmap')
my_map5  = get_map(location = c(lon = -87.69,lat = 41.87),maptype = 'roadmap', zoom = 11)
png("HeatMap2.png")
ggmap(my_map5,extent = 'device') + geom_point(shape = 16, data = both2_user, aes(x=longitude, y = latitude, colour = 100*pct, alpha = 0.01, 
                                     size = log(total)),show_guide = FALSE) +
    scale_colour_gradient(low = 'red', high = 'blue',limits = c(-10,10), 
                          name = "Net Bike Gain or Loss as % of Total Traffic",
                          labels = c("-10% or more", "-5%", "0%", "5%", "10% or more")) +
    theme(legend.position = c(0.3,0.2))
dev.off()
ggmap(my_map5,extent = 'device') + geom_point(shape = 16, data = both2_user, aes(x=longitude, y = latitude, colour = 100*pct, alpha = 0.01, 
                                                                                 size = log(total)),show_guide = FALSE) +
  scale_colour_gradient(low = 'red', high = 'blue',limits = c(-10,10),
                        guide = guide_legend(title  = "Net Bike Gain or Loss /n as % of Total Traffic",
                                             labels = c("-10% or more", "-5%", "0%", "5%", "10% or more")))  +
  theme(legend.position = c(0.3,0.2))








ggplot() + geom_point(data = both2_user, aes(x=longitude, y = latitude, colour = pct, alpha = 0.01, 
                                        size = log(total))) +
  scale_colour_gradient(low = 'red', high = 'blue',limits = c(-20,20)) +
  facet_grid(.~usertype) + theme_bw()




data_same <- sqldf("select count(*), usertype
                   from all_dat
                   where to_station_id = from_station_id
                   group by usertype")
