
###############################################################################
# this reads in Raed's rest name / address / risk level file and geocodes it
# since it didn't include lat lng.
###############################################################################

library(XLConnect)
library(ggmap)

risk <- readWorksheet(loadWorkbook("data/Retail Food 1006 Licenses 2013.xlsx"), 
                      sheet=1, startRow=3)
names(risk) <- tolower(names(risk))

# read in the inspections file from the data portal... it has license number 
# and lat lng. That should get most of these covered...
# data from Jan 1 2010 to present
ins <- read.csv("http://data.cityofchicago.org/views/4ijn-s7e5/rows.csv",
                stringsAsFactors = F)
names(ins) <- tolower(names(ins))
# let's look at the unique license numbers with valid lat lng
ins <- subset(ins, !is.na(latitude) | !is.na(longitude))
# only need license.. latitude longitude
ins <- subset(ins, select=c("license..", "latitude", "longitude"))
# remove duplicate license.. numbers
ins <- subset(ins, !duplicated(license..))

risk.merged <- merge(risk, ins, by.x="license.number", by.y="license..",
                     all.x=T)

# use the google API to get the missing lat lngs
# do this in a loop since we have to sleep to avoid google's rate limiting
risk.merged$lat <- risk.merged.lng <- rep(NA, nrow(risk.merged))
for(i in 1:nrow(risk.merged)){
  if(is.na(risk.merged$latitude[i])){
    rest.address <- with(risk.merged, paste0(facility.address[i], 
                      ", Chicago IL ", facility.zip[i])) 
    results <- try(geocode(rest.address))
    if(!inherits(results, "try-error")){
      risk.merged$lng[i] <- results$lon
      risk.merged$lat[i] <- results$lat
    }else{
      risk.merged$lng[i] <- NA
      risk.merged$lat[i] <- NA
    }
    Sys.sleep(.35) # can't ask google for these too fast...
  }
  if(sum(grepl("OVER_QUERY_LIMIT", warnings()))>0){
    break
  }
}

write.csv(risk.merged, "data/geocoded_risk_data.csv", row.names=F)