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
stateName <- read.csv("data/state_code.csv")
stateCodes <- str_pad(as.character(stateName[,2]), 2, "left", pad = " ")

endyear <- readline("Endyear: ")
nfactors <- readline("Number of Factors: ")

if (!file.exists(paste0("output/", endyear, "/useful_ACS_originalData.csv"))){
 stop("\nThere is no data available for this endyear at this geographical level.\nPlease run 'download_ACS_data.R' for this endyear and level")
}

createACSmat(endyear = endyear)
transform_vars(endyear = endyear, scale = T, keepdim = T)
factorAnalysis(nfactors = nfactors, endyear = endyear)
fa_bootstrap(nfactors = nfactors, endyear = endyear)
plotLoads(nfactors = nfactors, endyear = endyear)
myCFA(nfactors = nfactors, endyear = endyear)
fCorrplot(nfactors = nfactors, endyear = endyear)
MentalHealthCorr(nfactors = nfactors, endyear = endyear)