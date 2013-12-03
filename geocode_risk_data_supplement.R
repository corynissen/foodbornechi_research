
library(ggmap)

risk.merged <- read.csv("geocoded_risk_data.csv", stringsAsFactors=F)
risk.merged$lat[is.na(risk.merged$lng)] <- NA
risk.merged$lng[is.na(risk.merged$lat)] <- NA

for(i in 1:nrow(risk.merged)){
  if(is.na(risk.merged$latitude[i]) & is.na(risk.merged$lat[i])){
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
    results <- NULL
    Sys.sleep(.35) # can't ask google for these too fast...
  }
  if(sum(grepl("OVER_QUERY_LIMIT", warnings()))>0){
    break
  }
}

write.csv(risk.merged, "geocoded_risk_data2.csv", row.names=F)
