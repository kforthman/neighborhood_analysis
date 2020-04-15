# Download all needed ACS data.
require(acs)
require(stringr)
require(e1071)
require(psych)
require(DMwR)
require(randomcoloR)
require(corrplot)
require(ggplot2)
require(lavaan)
require(scales)
require(RColorBrewer)
require(GPArotation)

setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
source("R/functions.R")
api.key.install("591bda1a6151f1f125d11f35a2d5d8878a0df43b")

endyear <- readline("Endyear: ")
nfactors <- readline("Number of Factors: ")

state.codes <- as.matrix(read.csv("data/state_code.csv")[,3])

county.codes <- as.matrix(read.csv("data/county_code.csv")[,c(1,3)])
county.codes[,2] <- str_pad(as.numeric(county.codes[,2]), 3, "left", pad = "0")

useful_ACS_tables <- read.csv("data/useful_ACS_tables.csv", header = F)

saveAllUseful(endyear)
createOrgMega(endyear)
