library(acs)
library(tigris)
library(stringr)
library(leaflet)

# Developed by Katie Clary 03.31.2017
# Based on code from this website: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way

# Assigns the given name to each factor for the legend.
factorName_n <- function(factor){
  c("African\nAmericans\nin Tract", 
    "Seniors\nin Tract", 
    "Singletons\nin Tract", 
    "Affluence", 
    "Noncitizens\nin Tract")[as.numeric(substr(factor, 3,3))]
}

# Load the data.
setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
FA_Results <- readRDS("output/2015/5 factors/FA_5Factors.rds")
rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)


# The following code takes 2 inputs: state code and factor number. For example, to plot factor 1 in Oklahoma, you would call mapMyVar(40, 1).
mapMyVar <- function(sc.num, code){
  sc.char <- str_pad(as.character(sc.num), 2, "left", pad = 0)
  
  #tracts <- geo.make(state = sc.num, county = "*", tract = "*")
  tract.shape <- tracts(state = sc.num, county = NULL)
  
  
  # Any data that has geoCodes for rownames can be placed here. I have written it so that it takes the factor results for the specified factor and state from the table FA_Results.
  data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == sc.char), code]
  names(data) <- str_pad(names(data), 11, "left", pad="0")
  
  df <- data.frame(names(data), 
                   data, 
                   stringsAsFactors = FALSE)
  
  rownames(df) <- 1:nrow(df)
  names(df) <- c("GEOID", "var")
  
  merged <- geo_join(tract.shape, df, "GEOID", "GEOID")
  
  title <- factorName_n(colnames(FA_Results$scores)[code])
  
  popup <- paste0("GEOID: ", merged$GEOID, "<br>", title, " ", round(merged$var,2))
  # For a standard domain width, use:
  #pal <- colorNumeric(palette = "Spectral", domain = c(-4.4,4.4), reverse = T)
  # For a domain based on the range of the data use:
  pal <- colorNumeric(palette = "Spectral", domain = range(merged$var, na.rm = T), reverse = T)
  
  map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = merged,
                fillColor = ~pal(var),
                color = "#b2aeae",
                fillOpacity = 0.5,
                weight = 1,
                popup = popup) %>%
    addLegend(pal = pal,
              #values = c(-4.4,4.4),
              values = range(merged$var, na.rm = T),
              position = "bottomright",
              title = title)
  
  return(map)
}

ex_map <- mapMyVar(6, 1)
ex_map
