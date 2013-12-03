
###############################################################################
# this file reads in the geocoded_risk_data2.csv file and uses the lat lng
# and the CWFY API to get the ward number for each restaurant.
###############################################################################

library(RCurl)
library(RJSONIO)

rest.df <- read.csv("geocoded_risk_data2.csv", stringsAsFactors=F)

rest.df$lat.combined <- rest.df$latitude
rest.df$lat.combined[is.na(rest.df$lat.combined)] <- rest.df$lat[is.na(rest.df$lat.combined)]
rest.df$lng.combined <- rest.df$longitude
rest.df$lng.combined[is.na(rest.df$lng.combined)] <- rest.df$lng[is.na(rest.df$lng.combined)]

# example: "http://cwfy-api.smartchicagoapps.org/wards/boundaries.json?lat=41.8710&long=-87.6227
# returns: {"2013": 2,"2015": 4}
url1 <- "http://cwfy-api.smartchicagoapps.org/wards/boundaries.json?lat="
url2 <- "&long="

getWard <- function(lat, lng){
  ward.json <- getURL(paste0(url1, lat, url2, lng))
  ward.list <- try(fromJSON(ward.json))
  if(!inherits(ward.list, "try-error")){
    ward.2013 <- ward.list["2013"]
  }else{
    ward.2013 <- NA
  }
  return(ward.2013)
}

rest.df$ward <- as.character(mapply(getWard, rest.df$lat.combined, 
                                    rest.df$lng.combined))

write.csv(rest.df, "geocoded_risk_data_w_wards.csv", row.names=F)
