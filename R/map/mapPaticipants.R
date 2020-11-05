require(acs)
require(tigris)
require(stringr)
require(leaflet)
require(shiny)

setwd("/Volumes/T1000/Analysis/kforthman/Neighborhood_Analysis/Tract")

source('R/factorName.R')

sc.num = 40

sc.char <- str_pad(as.character(sc.num), 2, "left", pad = 0)

demo <- as.matrix(read.csv(paste0("output/2015/5 factors/loc_and_rw_and_score.csv")))
sub_id <- unique(demo[,'id'])

demo_single <- matrix(ncol=356,nrow=length(sub_id))
for(i in 1:length(sub_id)){
  this_id <- sub_id[i]
  this_demo <- which(demo[,'id']==this_id)
  this_demo <- demo[this_demo,]
  
  if(length(this_demo)>356){
    demo_single[i,] <- this_demo[1,]
  }
  else{
    demo_single[i,] <- this_demo
  }
  
}
colnames(demo_single) <- colnames(demo)

# These are the data for exclusively the T500 subjects. I have to merge the above data, which contains the lat/long data, with the subjects that are the T500 subjects.
demo2 <- as.matrix(read.csv(paste0("data/addresses/T500_factorScores_ 2018_Feb_08 .csv")))
sub_id2 <- unique(demo[,'id'])

T500_index <- is.element(demo_single[,'id'], demo2[,'id'])
ans <- demo_single[T500_index,]
# it appears 5 of the T500 subjects are missing addresses?


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

geocode <- str_pad(tract.shape$GEOID, 11, "left", pad="0")

popup <- paste0("GEOID: ", geocode)


map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = tract.shape,
              color = "red",
              fillOpacity = 0,
              weight = 1,
              popup = popup)


addCircles(map, 
           #lng = as.numeric(demo_single[,'longitude']),
           #lat = as.numeric(demo_single[,'latitude']),
           lng = jitter(as.numeric(demo_single[,'longitude']),3),
           lat = jitter(as.numeric(demo_single[,'latitude']),3),
           popup = paste(demo_single[,'givenAddress'], "<br>", demo_single[,'Income']),
           label = demo_single[,'id'],
           opacity = 1,
           fillOpacity = 1,
           fillColor = 'blue',
           weight = 3)

