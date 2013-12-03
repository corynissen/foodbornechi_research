
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

rest.df <- read.csv("geocoded_risk_data_w_wards.csv", stringsAsFactors=F)
# create data set w/ # of restaurants by ward
rest.summary <- ddply(rest.df, .(ward), summarize, count=length(ward))

# read the shapefile
shp <- readOGR("wards", "Wards")
shp <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))

# assign an ID to each polygon, and pull out the individual points
shp@data$id <- rownames(shp@data)
pts <- fortify(shp, region="id")

# merge in the other shapefile data
shp.df <- merge(pts, shp,by="id")

# merge in the crime data
shp.df <- merge(shp.df, rest.summary, by.x="WARD", by.y="ward")

# shp.df is all kinds of out of order right now. need to order by WARD,order
# or we end up with a broken polygon path
shp.df <- shp.df[order(shp.df$id, shp.df$order),]

# now for the graph
ggplot(shp.df,aes(long,lat,group=group)) + geom_polygon(aes(fill=count))

# same graph using ggmap package with google maps
p1 <- qmap("chicago", darken=.1) 
p2 <- p1 +geom_polygon(data=shp.df, aes(x=long, y=lat, group=group, fill=count), 
               color="white", size=.1) +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p2, "chi_restaurants.png", height=5, width=5)

p2 <- p1 + stat_density2d(data=rest.df, aes(x=lng.combined, y=lat.combined, 
      fill=..level..), geom="polygon") +
  scale_fill_gradient(low="gray1", high="gray99") +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), 
               fill="grey", color="white", size=.1, alpha=.2) +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p2, "chi_restaurants.png", height=5, width=5)

p2 <- p1 + stat_density2d(data=rest.df, aes(x=lng.combined, y=lat.combined, 
                                            fill=..density..), geom="tile", contour=F) +
  scale_fill_gradient(low="gray1", high="gray99") +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), 
               fill="grey", color="white", size=.1, alpha=.02) +
  coord_cartesian(xlim=c(-87.96, -87.5), ylim=c(41.62, 42.05))
ggsave(plot=p2, "chi_restaurants.png", height=5, width=5)

