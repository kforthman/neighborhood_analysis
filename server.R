library(acs)
library(tigris)
library(stringr)
library(leaflet)

#---------Given state, end year, table code, table row #, and generate map.
# endyear is a number (e.g. 2015),
# tcode is the code of the desired table as a string (e.g. "B17001"),
# rcode is the desired sub-table.
# Set ratio to TRUE if you want the proportion of the population/households rather 
# than the total.
# Developed by Katie Clary 03.31.2017
# Based on code from this website: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
#tracts <- geo.make(state = sc.num, county = "*", tract = "*")

# must install key to use census data.
# only need to do once
# api.key.install(key="591bda1a6151f1f125d11f35a2d5d8878a0df43b") 

factorName_n <- c("African\nAmericans\nin Tract", 
                  "Seniors\nin Tract", 
                  "Singletons\nin Tract", 
                  "Affluence", 
                  "Noncitizens\nin Tract")

function(input, output){
  FA_Results <- readRDS("FA_5Factors.rds")
  rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)
 
   output$map <- renderLeaflet({ 
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # tract.shape <- reactive({
  #   readRDS(paste0("tracts/tractSPDF_", input$sc.num,".rds"))#, cb = TRUE)#tracts(state = as.numeric(sc.char), county = NULL) 
  # })

  observe({
    
    tract.shape <- readRDS(paste0("tracts/tractSPDF_", input$sc.num,".rds"))#, cb = TRUE)#tracts(state = as.numeric(sc.char), county = NULL) 
    data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == input$sc.num), as.numeric(input$code)]
    df <- data.frame(names(data), 
                     data, 
                     stringsAsFactors = FALSE)
    
    rownames(df) <- 1:nrow(df)
    names(df) <- c("GEOID", "var")
    
    title <- factorName_n[as.numeric(input$code)]
    
    merged <- geo_join(tract.shape, df, "GEOID", "GEOID")
    
    popup <- paste0("GEOID: ", merged$GEOID, "<br>", title, " ", round(merged$var,2))
    #pal <- colorNumeric(palette = "Spectral", domain = c(-4.4,4.4), reverse = T)
    pal <- colorNumeric(palette = "Spectral",
                        domain = round(merged$var, 2),
                        reverse = T)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = merged,
                  fillColor = ~pal(var),
                  color = "#b2aeae",
                  fillOpacity = 0.5,
                  weight = 1,
                  popup = popup) %>%
      addLegend(pal = pal,
                values = round(merged$var, 2),
                position = "bottomright",
                title = title) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
}