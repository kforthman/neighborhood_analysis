# address2tract
# This code has the capability of assigning a tract number given an address.
# ------------------------------------------------------------------------------
library(RCurl)
library(RJSONIO)
library(maptools)
library(acs)

# The key makes it possible to download census data.
key <- "591bda1a6151f1f125d11f35a2d5d8878a0df43b"


setwd("~/Dropbox (LIBR)/Neighborhood Analysis")


url <- function(address, return.call = "json", sensor = "false") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# The geoCode function takes an address and returns the latitude and longitude.
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

# Using latitude and longitude, tractLookup finds the tract in which the coordinates reside.
tractLookup <- function(x, y, state) {
  pt <- SpatialPoints(data.frame(x = x, y = y))
  overlay.pt <- over(pt, state) # what index number does pt fall inside?
  return(overlay.pt) 
  #  return(as.character(overlay.pt$TRACTCE10)) # give he Tract number from the census layer
}

# Geographical information of the area must be downloaded.
oklahoma <- readShapePoly("tl_2010_40_tract10/tl_2010_40_tract10.shp")

# Example: an address is assigned a tract number.
address <- geoCode("4010 N Cadillac dr, Fayetteville, AR 72703")
tractLookup(as.numeric(address[2]),as.numeric(address[1]), oklahoma)


# Here, we will assign a tract to all participants in the T1000 dataset
# ------------------------------------------------------------------------------
# Subject addresses are directly taken from subject input.
sub_addresses <- as.matrix(read.csv("addresses/all_address.csv"))
# The cleaned addresses have been manually corrected by Clary, one by one.
sub_addresses_c <- as.matrix(read.csv("addresses/all_address_cleaned.csv"))

# If am address has been manually corrected, it replaces the subject-input address.
sub_addresses[which(!(sub_addresses_c[,'manualAddress'] == '')), 5] <- sub_addresses_c[which(!(sub_addresses_c[,'manualAddress'] == '')), 2]

nsub <- dim(sub_addresses)[1]
sub_geoCodes <- matrix(nrow=nsub,ncol=4)
rownames(sub_geoCodes) <- sub_addresses[,'record_id']

for(i in 1:nsub){
  if (!is.na(sub_geoCodes[i,1])){
    next
  }
  if (sub_addresses[i,5] == ''){
    next
  }
  print(i)
  #Sys.sleep(0.3)
  address <- geoCode(sub_addresses[i,'address'])
  sub_geoCodes[i,] <- address
}

write.csv(sub_geoCodes, "sub_geoCodes.csv")


sub_tract <- matrix(nrow = nsub, ncol = 1)
rownames(sub_tract) <- sub_addresses[,'record_id']

for(i in 1:nsub){
  if (is.na(sub_geoCodes[i,1])){
    next
  }
  if (sub_addresses[i,5] == ''){
    next
  }
  print(i)
  tract <- as.matrix(tractLookup(as.numeric(sub_geoCodes[i,2]),as.numeric(sub_geoCodes[i,1]), oklahoma))
  sub_tract[i,1] <- tract[1,4]
}

write.csv(sub_tract, "sub_tract.csv")

sub_tract <- read.csv("addresses/sub_tract.csv")
