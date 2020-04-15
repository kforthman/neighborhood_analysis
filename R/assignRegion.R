library(stringr)
# Here state regions are assigned to the U.S. census tracts and their factor 
# scores. The resulting table will make it possible to observe how factor
# scores differ by region
#
#
# Created by Katie Forthman, 04/02/2019


# Load the data.
setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
FA_Results <- readRDS("output/2015/5 factors/FA_5Factors.rds")
rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)

state_region <- read.csv("data/state_region.csv")
state_region[,2] <- str_pad(state_region[,2], 2, "left", pad = 0)

# Assign State to each tract
data <- FA_Results$scores
data <- data.frame(data)
data$state_code <- substr(rownames(data), 1, 2)
data <- merge(data, state_region, by.x = 6, by.y = 2)

# Output the resulting table
write.csv(data, "output/2015/5 factors/FA_5factors_regionAssigned.csv")
