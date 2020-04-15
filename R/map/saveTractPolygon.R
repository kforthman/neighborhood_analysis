stateName <- read.csv("state_code.csv")
stateCodes <- str_pad(as.character(stateName[,2]), 2, "left", pad = "0")


for(i in 1:dim(stateName)[1]){
  tract.shape <- tracts(state = as.numeric(stateCodes[i]), county = NULL)
  saveRDS(tract.shape, paste0("tracts/tractSPDF_",stateCodes[i], ".rds"))
}