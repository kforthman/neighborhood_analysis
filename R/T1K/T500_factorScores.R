setwd("~/Dropbox (LIBR)/Neighborhood Analysis")

T500_address <- as.matrix(read.csv("T500_addresses_17_11_02.csv"))
T500_address[which(substr(T500_address[,8],1,1) == " "), 8] <- substr(T500_address[which(
  substr(T500_address[,8],1,1) == " "), 8],2,11) # removes id's with space at the beginning

FA_results <- readRDS("fa results/FA_5factors.RDS")
scores <- cbind(rownames(FA_results$scores),FA_results$scores)

merged <- merge(T500_address, scores, by.x = 8, by.y = 1)

factorName <- function(factor){
  c("African Americans in Tract", "Seniors in Tract", "Singletons in Tract", "Affluence", "Noncitizens in Tract")[as.numeric(substr(factor, 3,3))]
}

colnames(merged)[9:13] <- factorName(colnames(merged)[9:13])

merged <- merged[,c(2:8,1,9:13)]

write.csv(merged, paste("T500_factorScores_",format(Sys.time(), "%Y_%b_%d"),".csv"), row.names = F)
