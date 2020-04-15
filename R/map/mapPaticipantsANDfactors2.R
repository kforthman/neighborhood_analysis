require(acs)
require(tigris)
require(stringr)
require(leaflet)
require(shiny)

setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")

source('R/factorName.R')


mapMyVar <- function(sc.num, myCode, set = "all", nfactors = 5,endyear = 2015, addParticipants = F){
  sc.char <- str_pad(as.character(sc.num), 2, "left", pad = 0)
  
  
  FA_Results <- readRDS(paste0("output/",endyear,"/",
                               nfactors," factors/FA_",nfactors,"Factors.rds"))
  rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)
  
  demo <- as.matrix(read.csv(paste0("output/",endyear,"/",
                                    nfactors," factors/loc_and_rw_and_score.csv")))
  
  pal <- colorNumeric(palette = "Spectral",
                      domain = round(FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == sc.char), myCode], 2),
                                     reverse = T)
  # pal <- colorNumeric(palette = "Spectral",
  #                     domain = seq(-4,4, by = 0.01),
  #                                    reverse = T)
  
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
    
    title <- factorName(colnames(FA_Results$scores)[code])
    setTxtProgressBar(pb, 7) #progress#
    
    popup <- paste0("GEOID: ", merged$GEOID, "<br>", title, " ", round(merged$var,2))
    
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
                values = merged$var, #c(-4,4),
                position = "bottomright",
                title = title)
    setTxtProgressBar(pb, 10) #progress#
    
    return(map)
  }
  
  ## acs.lookup(endyear = 2015, table.number = 25075)
  
  ex_map <- mapACS(myCode)
  # ex_map
  # 
  # # Plots each participant colored by affluence score. Serves to make sure that affluence 
  # # scores have been assigned correctly.
  # addCircles(ex_map, lng = as.numeric(demo[,'longitude']),
  #            lat = as.numeric(demo[,'latitude']), 
  #            popup = paste(demo[,'givenAddress'], "<br>", demo[,'Affluence']),
  #            label = demo['id'], 
  #            opacity = 1, 
  #            fillOpacity = 1, 
  #            fillColor = 'black', 
  #            color = pal(round(as.numeric(demo[,'Affluence']), 2)))
  # aff <- as.numeric(na.omit(demo[,c('Income', 'Affluence')])[,'Affluence'])
  # inc <- as.numeric(na.omit(demo[,c('Income', 'Affluence')])[,'Income'])
  # plot(aff,inc)
  # abline(lm(inc~aff), col="red")
  # cor(aff, inc, method = 'spearman')
  # plot(aff,log(inc+500))
  # abline(lm(log(inc+500)~aff), col="red")
  # cor(aff, log(inc+500), method = 'spearman')
  ex_map

if(addParticipants){
  pal2 <- colorNumeric(palette = "Spectral", domain = log(as.numeric(na.omit(demo[,'Income']))+500), reverse = T)

  addCircles(ex_map, lng = as.numeric(demo[,'longitude']),
             lat = as.numeric(demo[,'latitude']),
             popup = paste(demo[,'givenAddress'], "<br>", demo[,'Income']),
             label = demo[,'id'],
             opacity = 1,
             fillOpacity = 1,
             fillColor = 'black',
             color = pal2(log(as.numeric(demo[,'Income'])+500)))
  }
}

mapMyVar(40, 5, addParticipants = T)
# mapMyVar(40, 2)
# mapMyVar(40, 3)
# mapMyVar(40, 4)
# mapMyVar(40, 5)
