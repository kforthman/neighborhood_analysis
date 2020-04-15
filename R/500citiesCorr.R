# Correlate 500 cities features to the factor scores.

setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
source("R/functions.R")


cdc_data <- read.csv("data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format_.csv")
cdc_data_names <- as.matrix(read.csv("data/500-Cities VAR CODES.csv", header = F))

library(corrplot)

cdc_data_var <- cdc_data[c(4,seq(7, 62, by = 2))]
#colnames(cdc_data_var)[2:dim(cdc_data_var)[2]] <- sapply(2:dim(cdc_data_var)[2], function(x){substr(colnames(cdc_data_var)[x], 1, nchar(colnames(cdc_data_var)[x])-10)})
colnames(cdc_data_var)[2:dim(cdc_data_var)[2]] <- cdc_data_names[,3]
cdc_data_var[,1] <- str_pad(as.character(cdc_data_var[,1]), 11, "left", pad = 0)

library(DMwR)
cdc_data_var[,2:dim(cdc_data_var)[2]] <- knnImputation(cdc_data_var[,2:dim(cdc_data_var)[2]])

t1 <- cdc_data_var

# Add in the factor scores
endyear <- 2015
nfactors <- 5
FA_Results <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
factor_scores <- cbind(rownames(FA_Results$scores), FA_Results$scores)
factor_scores[,1] <- str_pad(as.character(factor_scores[,1]), 11, "left", pad = 0)
t2 <- factor_scores

# merge
merged <- as.matrix(merge(t1,t2, by = 1))
tractCodes <- merged[,1]
colnames(merged)[30:34] <- sapply(colnames(merged)[30:34], function(x){factorName(x, n = F)})
merged <- apply(merged[,2:dim(merged)[2]], 2, as.numeric)
#merged <- cbind(tractCodes, merged)

#write.csv(merged, "data/500-Cities with Factor Scores.csv")

merged <- merged[,c(9:11,13,14,16,21,24,1:8,12,15,17:20,22,23,25:33)]
# corrplt
merged_cor <- cor(merged)

d <- dim(merged_cor)[2]
corrplot(merged_cor[(d-4):d,1:(d-5)], method = "ellipse")


#"500cities-factor-corrplot"

# # FA for some reason
# nfac <- 1
# citiesFA <- fa(cdc_data_var[,-1],nfac)
# x = seq(.6,length = 28,by = 1.2)
# for(i in 1:nfac){
#   cols <- ifelse(abs(citiesFA$weights[,i])>.3,"red", "pink")
#   cols2 <- ifelse(abs(citiesFA$weights[,i])>.3,"black", "grey")
# barplot(citiesFA$weights[,i], names.arg = "", xpd=TRUE, border=NA, horiz = T, xlim = c(-1,1), col=cols)
# poss <- ifelse(citiesFA$weights[,i]>0,2, 4)
#   text(cex=1, x=0, y=x, labels = rownames(citiesFA$weights), xpd=TRUE, srt=0, pos = poss, col=cols2)
# }

