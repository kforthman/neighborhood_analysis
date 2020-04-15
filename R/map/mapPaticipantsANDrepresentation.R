library(acs)
library(tigris)
library(stringr)
library(leaflet)


# This code creates a map which highlights the T500 representation of each tract. The color of a tract indicates how many of the T500 subjects are living in that tract. If a tract is clicked, a window will pop up that gives the geoID and the exact number of participants in that tract.
# Developed by Katie Forthman 01.31.2019
# Based on code from this website: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way

setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")

sc.num <- 40

sc.char <- str_pad(as.character(sc.num), 2, "left", pad = 0)
demo <- as.matrix(read.csv(paste0("data/addresses/T500_factorScores_ 2018_Feb_08 .csv")))

sub_id <- unique(demo[,'id'])

demo_single <- matrix(ncol=13,nrow=500)
for(i in 1:length(sub_id)){
  this_id <- sub_id[i]
  this_demo <- which(demo[,'id']==this_id)
  this_demo <- demo[this_demo,]
  
  if(length(this_demo)>13){
    demo_single[i,] <- this_demo[1,]
  }
  else{
    demo_single[i,] <- this_demo
  }
  
}
colnames(demo_single) <- colnames(demo)

demo <- demo_single

repres <- table(demo[,'geoCode'])
rownames(repres) <- str_pad(rownames(repres), 11, "left", pad = 0)

# must install key to use census data.
# only need to do once
##api.key.install(key="591bda1a6151f1f125d11f35a2d5d8878a0df43b") 

tracts <- geo.make(state = sc.num, county = "*", tract = "*")
tract.shape <- tracts(state = sc.num, county = NULL)#, cb = TRUE)


data <- repres

df <- data.frame(data, 
                 stringsAsFactors = FALSE)

rownames(df) <- 1:nrow(df)
names(df) <- c("GEOID", "var")

merged <- geo_join(tract.shape, df, "GEOID", "GEOID")

n_part <- merged$var
n_part[which(is.na(merged$var))] <- 0
popup <- paste0("GEOID: ", merged$GEOID, "<br>", "Number Participants in Tract: ", n_part)
pal <- colorNumeric(palette = "Spectral", domain = merged$var, reverse = T)

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
            title = "Number Participants in Tract")

map

