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

library(lme4)
library(lmerTest)
library(EValue)
library(sjPlot)

# Perform an LME to examine confounders.

setwd("/Volumes/T1000/Analysis/kforthman/Neighborhood_Analysis/Tract")

# factor scores
facts <- read.csv("output/2015/5\ factors/FA_5Factors.csv")
facts[,1] <- str_pad(facts[,1], 11, "left", pad="0")

# BRFSS data
cdc_data <- read.csv("data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format_.csv")
cdc_data[,4] <- str_pad(as.character(cdc_data[,4]), 11, "left", pad = 0)
mh_data <- cdc_data[,c("TractFIPS","MHLTH_CrudePrev")]
cdc_data <- cdc_data[,!(colnames(cdc_data) %in% c("X", "PlaceName", "PlaceFIPS", "Place_TractID", "population2010", "Geolocation", "MHLTH_CrudePrev"))]
cdc_data <- cdc_data[,!grepl("_Crude95CI", colnames(cdc_data))]

pca_health <- pca(cdc_data[,-1], nfactors = ncol(cdc_data) - 1)
pca_data <- pca_health$scores
pca_data <- as.data.frame(pca_data)
pca_data$tract <- cdc_data[,1]

print(pca_health$loadings)

# states
stateName <- read.csv("data/state_code.csv")
stateName[,2] <- str_pad(as.character(stateName[,2]), 2, "left", pad = "0")

facts$state <- substr(facts$X, 1, 2)
facts$stateName <- stateName[match(facts$state, stateName[,2]),3]

# put it all together
all.data <- data.frame(
  state = facts$stateName,
  facts[,c("Affluence", "Singletons.in.Tract", "Seniors.in.Tract", "African.Americans.in.Tract", "Noncitizens.in.Tract")],
  MHLTH_CrudePrev = mh_data[match(facts$X, mh_data$TractFIPS), "MHLTH_CrudePrev"],
  pca_data[match(facts$X, pca_data$tract), !(colnames(pca_data) %in% "tract")]
)

# Do the LME
this_lme <- lmer('MHLTH_CrudePrev ~ Affluence + Singletons.in.Tract + Seniors.in.Tract + African.Americans.in.Tract + Noncitizens.in.Tract +  
                 RC1 + RC2 + RC3 + RC4 +
                 (1|state)', data = all.data)
this_lme_sum <- summary(this_lme)
# e values

eval <- data.frame(factor_name = c("Affluence", "Singletons.in.Tract", "Seniors.in.Tract", "African.Americans.in.Tract", "Noncitizens.in.Tract"))
for(fac in c("Affluence", "Singletons.in.Tract", "Seniors.in.Tract", "African.Americans.in.Tract", "Noncitizens.in.Tract",
             "RC1", "RC2", "RC3", "RC4")){
  print(fac)
print(evalues.OLS(
  est = this_lme_sum$coefficients[fac,"Estimate"],
  se = this_lme_sum$coefficients[fac,"Std. Error"],
  sd = sd(all.data[,fac], na.rm = T)
))
message("\n\n")
  }

# forest plot
plot_model(this_lme)
