
###############################################################################
# this file creates several maps using the restaurant data and inspection
# data and the ward boundary shapefiles
###############################################################################

library(rgdal)
library(ggmap)
library(plyr)
library(ggplot2)
library(maptools)
gpclibPermit()

rest.df <- read.csv("data/geocoded_risk_data_w_wards.csv", stringsAsFactors=F)
# create data set w/ # of restaurants by ward
rest.summary <- ddply(rest.df, .(ward), summarize, count=length(ward))
# same data set by risk level
rest.summary2 <- ddply(rest.df, .(ward, risk.category), summarize, 
                       count=length(ward))
rest.summary3 <- merge(rest.summary2, rest.summary, by.x="ward", by.y="ward")
rest.summary3$perc <- (rest.summary3$count.x / rest.summary3$count.y) * 100

# read the shapefile
shp <- readOGR("wards", "Wards")
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
# assign an ID to each polygon, and pull out the individual points
shp@data$id <- rownames(shp@data)
pts <- fortify(shp, region="id")
# merge in the other shapefile data
shp.df <- merge(pts, shp,by="id")
# merge in the restaurant data
allrisk <- merge(shp.df, rest.summary, by.x="WARD", by.y="ward")
allrisk <- allrisk[order(allrisk$id, allrisk$order),]
# and the same data set by risk level
byrisk <- merge(shp.df, rest.summary3, by.x="WARD", by.y="ward")
byrisk <- byrisk[order(byrisk$id, byrisk$order),]

# all risk levels; graph using ggmap package with cloudmade maps
cloudmadekey <- scan("~/cn/personal/keys/cloudmadekey.txt", what="character")
p1 <- qmap("chicago", source="cloudmade", api_key=cloudmadekey, maptype=108995)
p2 <- p1 +geom_polygon(data=allrisk, 
  aes(x=long, y=lat, group=group, fill=count), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Restaurant\nCount", low="#001F4C", high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p2, "output/chi_restaurant_count_by_ward.png", height=5, width=5)

# all risk levels; take out ward 42 and redraw...
allrisk2 <- allrisk
allrisk2$count[allrisk2$WARD==42] <- NA
p3 <- p1 +geom_polygon(data=allrisk2, 
  aes(x=long, y=lat, group=group, fill=count), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Restaurant\nCount", low="#001F4C", high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p3, "output/chi_restaurant_count_by_ward2.png", height=5, width=5)

# all risk levels; gradient map
p4 <- p1 + stat_density2d(data=rest.df,   
  aes(x=lng.combined, y=lat.combined, fill=..density..), geom="tile", 
    contour=F) +
  scale_fill_gradient(low="gray1", high="gray99") +
  theme(legend.position=c(.9, .83)) + 
  geom_polygon(data=allrisk, aes(x=long, y=lat, group=group), 
    fill="grey", color="white", size=.15, alpha=.02) +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p4, "output/chi_restaurant_count_gradient.png", height=5, width=5)

# risk level 1 (high); map by ward
byrisk.high <- subset(byrisk, risk.category=="Risk 1 (High)")
p5 <- p1 +geom_polygon(data=byrisk.high, 
  aes(x=long, y=lat, group=group, fill=count.x), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="High Risk\nRestaurant\nCount", low="#001F4C", 
    high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p5, "output/chi_restaurant_count_highrisk_by_ward.png", height=5, 
  width=5)

# risk level 2 (medium); map by ward
byrisk.med <- subset(byrisk, risk.category=="Risk 2 (Medium)")
p6 <- p1 +geom_polygon(data=byrisk.med, 
  aes(x=long, y=lat, group=group, fill=count.x), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Medium Risk\nRestaurant\nCount", low="#001F4C", 
    high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p6, "output/chi_restaurant_count_medrisk_by_ward.png", height=5, 
  width=5)

# risk level 3 (low); map by ward
byrisk.low <- subset(byrisk, risk.category=="Risk 3 (Low)")
p7 <- p1 +geom_polygon(data=byrisk.low, 
  aes(x=long, y=lat, group=group, fill=count.x), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Low Risk\nRestaurant\nCount", low="#001F4C", 
    high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p7, "output/chi_restaurant_count_lowrisk_by_ward.png", height=5, 
  width=5)

# risk level 1 (high); percentage map by ward
p8 <- p1 +geom_polygon(data=byrisk.high, 
  aes(x=long, y=lat, group=group, fill=perc), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="High Risk\nRestaurant\nPercentage", low="#001F4C", 
                      high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p8, "output/chi_restaurant_percentage_highrisk_by_ward.png", 
  height=5, width=5)

# risk level 2 (medium); percentage map by ward
p9 <- p1 +geom_polygon(data=byrisk.med, 
  aes(x=long, y=lat, group=group, fill=perc), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Medium Risk\nRestaurant\nPercentage", low="#001F4C", 
    high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p9, "output/chi_restaurant_percentage_medrisk_by_ward.png", 
  height=5, width=5)

# risk level 3 (low); percentage map by ward
p10 <- p1 +geom_polygon(data=byrisk.low, 
  aes(x=long, y=lat, group=group, fill=perc), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Low Risk\nRestaurant\nPercentage", low="#001F4C", 
    high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p10, "output/chi_restaurant_percentage_lowrisk_by_ward.png", 
  height=5, width=5)

