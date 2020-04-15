setwd("/Volumes/T1000/Analysis/kclary/Neighborhood_Analysis/Tract")
FA_Results <- readRDS("output/2015/5 factors/FA_5Factors.rds")
rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)

stateName <- read.csv("data/state_code.csv")
stateCodes <- str_pad(as.character(stateName[,2]), 2, "left", pad = "0")

state_avg <- matrix(nrow = length(stateCodes), ncol = 5)
colnames(state_avg) <- colnames(FA_Results$scores)
for(i in 1:length(stateCodes)){
  data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == stateCodes[i]), ]
  state_avg[i,] <- apply(data, 2, mean)
}

state_avg <- cbind(stateCodes, state_avg)


countyName <- read.csv("data/county_code.csv")
countyCodes <- cbind(str_pad(as.character(countyName[,2]), 2, "left", pad = "0"),
                     str_pad(as.character(countyName[,3]), 3, "left", pad = "0"))


county_avg <- matrix(nrow = dim(countyCodes)[1], ncol = 5)
colnames(county_avg) <- colnames(FA_Results$scores)
for(j in 1:length(countyCodes)){
  print(j)
  data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == countyCodes[j,1]), ]
  data <- data[which(substr(rownames(data), 3,5) == countyCodes[j,2]), ]
  
  if(is.null(dim(data))){
    county_avg[j,] <-data
  }else{
    county_avg[j,] <- apply(data, 2, mean)
  }
  
}

county_avg <- cbind(countyCodes, county_avg)
colnames(county_avg)[1:2] <- c("state_code", "county_code")
