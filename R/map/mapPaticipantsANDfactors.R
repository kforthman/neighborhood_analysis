library(acs)
library(tigris)
library(stringr)
library(leaflet)
factorName_n <- function(factor){
  c("African\nAmericans\nin Tract", 
    "Seniors\nin Tract", 
    "Singletons\nin Tract", 
    "Affluence", 
    "Noncitizens\nin Tract")[as.numeric(substr(factor, 3,3))]
}

setwd("~/Dropbox (LIBR)/Neighborhood Analysis")
income <- as.matrix(read.csv("addresses/T1000_redcap_wide-2018-03-21.csv")[,c('id','visit', 'Income')])
income <- income[which(income[,2] == 'S'),c(1,3)]
scores <- as.matrix(read.csv("T500_factorScores.csv")[,c('id','Affluence')])

mapMyVar <- function(sc.num, myCode, set = "all", nfactors = 5){
  sc.char <- str_pad(as.character(sc.num), 2, "left", pad = 0)
  
  setwd("~/Dropbox (LIBR)/Neighborhood Analysis")
  FA_Results <- readRDS("fa results/FA_5Factors.rds")
  rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)
  # must install key to use census data.
  # only need to do once
  ##api.key.install(key="591bda1a6151f1f125d11f35a2d5d8878a0df43b") 
  
  #---------Given state, end year, table code, table row #, and generate map.
  # endyear is a number (e.g. 2015),
  # tcode is the code of the desired table as a string (e.g. "B17001"),
  # rcode is the desired sub-table.
  # Set ratio to TRUE if you want the proportion of the population/households rather 
  # than the total.
  # Developed by Katie Clary 03.31.2017
  # Based on code from this website: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
  tracts <- geo.make(state = sc.num, county = "*", tract = "*")
  tract.shape <- tracts(state = sc.num, county = NULL)#, cb = TRUE)
  
  mapACS <- function(code){
    pb <- txtProgressBar(min = 0, max = 10, style = 3) #progress#
    setTxtProgressBar(pb, 1) #progress#
    
    data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == sc.char), code]
    names(data) <- str_pad(names(data), 11, "left", pad="0")
    setTxtProgressBar(pb, 2) #progress#
    
    df <- data.frame(names(data), 
                     data, 
                     stringsAsFactors = FALSE)
    setTxtProgressBar(pb, 4) #progress#
    
    rownames(df) <- 1:nrow(df)
    names(df) <- c("GEOID", "var")
    setTxtProgressBar(pb, 5) #progress#
    
    merged <- geo_join(tract.shape, df, "GEOID", "GEOID")
    setTxtProgressBar(pb, 6) #progress#
    
    title <- factorName_n(colnames(FA_Results$scores)[code])
    setTxtProgressBar(pb, 7) #progress#
    
    popup <- paste0("GEOID: ", merged$GEOID, "<br>", title, " ", round(merged$var,2))
    pal <- colorNumeric(palette = "Spectral", domain = merged$var, reverse = T)
    setTxtProgressBar(pb, 8) #progress#
    
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = merged,
                  fillColor = ~pal(var),
                  color = "#b2aeae",
                  fillOpacity = 0.6,
                  weight = 1,
                  popup = popup) %>%
      addLegend(pal = pal,
                values = merged$var,
                position = "bottomright",
                title = title)
    setTxtProgressBar(pb, 10) #progress#
    
    return(map)
  }
  
  ## acs.lookup(endyear = 2015, table.number = 25075)
  
  ex_map <- mapACS(myCode)
  ex_map
  
  
  setwd("addresses")
  
  latlong <- read.csv('sub_geoCodes.csv')
  all_address <- read.csv('all_address.csv')
  
  dim(latlong)[1] == length(which(all_address[,1] == latlong[,1]))
  
  latlong[,4] <- paste(latlong[,4], '<br>', all_address[,5])
  
  latlongandincome <- merge(latlong, income, by = c(1,1))
  
  latlongandincome <- latlongandincome[which(!is.na(latlongandincome[,2])),]
  
  pal2 <- colorNumeric(palette = "Spectral", domain = as.numeric(latlongandincome[,6]), reverse = T)
  
  addCircles(ex_map, lng = latlongandincome[,3], lat = latlongandincome[,2], popup = paste(latlongandincome[,4],"<br>", latlongandincome[,6]),
             label = latlongandincome[,1], opacity = 1, fillOpacity = 1, fillColor = 'black', 
             color = pal2(as.numeric(latlongandincome[,6])))
}

mapMyVar(40, 1)
# mapMyVar(40, 2)
# mapMyVar(40, 3)
# mapMyVar(40, 4)
# mapMyVar(40, 5)
