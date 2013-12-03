
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

# read the shapefile
shp <- readOGR("wards", "Wards")
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
# assign an ID to each polygon, and pull out the individual points
shp@data$id <- rownames(shp@data)
pts <- fortify(shp, region="id")
# merge in the other shapefile data
shp.df <- merge(pts, shp,by="id")
# merge in the restaurant data
shp.df <- merge(shp.df, rest.summary, by.x="WARD", by.y="ward")
# shp.df is all kinds of out of order right now. need to order by WARD,order
# or we end up with a broken polygon path
shp.df <- shp.df[order(shp.df$id, shp.df$order),]

# graph using ggmap package with cloudmade maps
cloudmadekey <- scan("~/cn/personal/keys/cloudmadekey.txt", what="character")
p1 <- qmap("chicago", source="cloudmade", api_key=cloudmadekey, maptype=108995)
p2 <- p1 +geom_polygon(data=shp.df, 
  aes(x=long, y=lat, group=group, fill=count), color="white", size=.15) + 
  theme(legend.position=c(.9, .83)) + 
  scale_fill_gradient(name="Restaurant\nCount", low="#001F4C", high="#E6F0FF") +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p2, "output/chi_restaurant_count_by_ward.png", height=5, width=5)

p3 <- p1 + stat_density2d(data=rest.df,   
  aes(x=lng.combined, y=lat.combined, fill=..density..), geom="tile", 
    contour=F) +
  scale_fill_gradient(low="gray1", high="gray99") +
  theme(legend.position=c(.9, .83)) + 
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), 
    fill="grey", color="white", size=.15, alpha=.02) +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p3, "output/chi_restaurant_count_gradient.png", height=5, width=5)

