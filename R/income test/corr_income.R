# Looks at the correlation between tract Affluence, tract average income, and T1K subject income.

# _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
# Is tract affluence highly correlated to tract income?

require(stringr)
setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
source("R/functions.R")
stateName <- read.csv("data/state_code.csv")
stateCodes <- str_pad(as.character(stateName[,2]), 2, "left", pad = " ")

endyear <- readline("Endyear: ")
nfactors <- readline("Number of Factors: ")

if (!file.exists(paste0("output/", endyear, "/useful_ACS_originalData.csv"))){
  stop("\nThere is no data available for this endyear.\nPlease run 'download_ACS_data.R' for this endyear")
}

cond_Data <- read.csv(paste0("output/", endyear, "/useful_ACS_condensedData.csv"))[,c('X', 'Income')]
FA_Data <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))$scores
colnames(FA_Data) <- factorName(colnames(FA_Data), n = F)
FA_Data <- as.matrix(cbind(FA_Data, rownames(FA_Data)))


tract_a2i <- merge(cond_Data, FA_Data, by.x = 1, by.y = 6)
tract_a2i[,1] <- str_pad(as.character(tract_a2i[,1]), 11, "left", pad = "0")
tract_a2i <- tract_a2i[which(substr(tract_a2i[,1], 1, 2) == "40"),]
# plot(tract_a2i[,"Affluence"], tract_a2i[,"Income"]) # Too slow. DON'T TRY THIS.

aff <- as.numeric(as.matrix(tract_a2i[,"Affluence"]))
inc <- tract_a2i[,"Income"]

library(hexbin)
hexbinplot(inc ~ aff, type="r", xlab = "Tract Affluence", ylab = "Tract Average Income")
cor(aff, inc, method = 'spearman')

# _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
# Is tract affluence highly correlated with subject income?

demo <- as.matrix(read.csv(paste0("output/",endyear,"/",
                                  nfactors," factors/loc_and_rw_and_score.csv")))
aff <- as.numeric(na.omit(demo[,c('Income', 'Affluence')])[,'Affluence'])
inc <- as.numeric(na.omit(demo[,c('Income', 'Affluence')])[,'Income'])
plot(aff,inc, xlab = "Tract Affluence", ylab = "Subject Income")
abline(lm(inc~aff), col="red")
cor(aff, inc, method = 'spearman')
plot(aff,log(inc+500), xlab = "Tract Affluence", ylab = "Log(Subject Income)")
abline(lm(log(inc+500)~aff), col="red")
cor(aff, log(inc+500), method = 'spearman')

# _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
# Is tract average income highly correlated with subject income?

subi2traci <- merge(demo, tract_a2i, by.x = 2, by.y = 1)[,c("id","event","Income.x","Income.y")]
subi2traci[,3] <- as.numeric(as.matrix(subi2traci[,3]))
s_inc <- na.omit(subi2traci[,c('Income.x', 'Income.y')])[,'Income.x']
t_inc <- na.omit(subi2traci[,c('Income.x', 'Income.y')])[,'Income.y']
plot(t_inc, s_inc, xlab = "Tract Average Income", ylab = "Subject Income", ylim = c(0, 200000))
abline(lm(s_inc~t_inc), col="red")
cor(t_inc, s_inc, method = 'spearman')

