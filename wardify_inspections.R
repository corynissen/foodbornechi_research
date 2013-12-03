
###############################################################################
# this file reads in the food inspections from the data portal from Jan 2010
# to present and uses the SC app cwfy to get the ward numbers for them
###############################################################################

library(RCurl)
library(RJSONIO)

# data from Jan 1 2010 to present
ins <- read.csv("http://data.cityofchicago.org/views/4ijn-s7e5/rows.csv",
                 stringsAsFactors = F)
names(ins) <- tolower(names(ins))
# let's look at the unique license numbers with valid lat lng
ins <- subset(ins, !is.na(latitude) | !is.na(longitude))

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

ins$ward <- mapply(getWard, ins$latitude, ins$longitude)

write.csv(ins, "inspections_with_ward.csv", row.names=F)
