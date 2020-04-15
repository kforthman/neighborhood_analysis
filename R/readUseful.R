# A function for reading in the saved ACS files.
readUseful <- function(state, endyear, tablename){
  filename <- paste0("data/ACS_data/", as.character(endyear), "/", state, "/", tablename, ".rds")
  ifelse(file.exists(filename), return(readRDS(filename)), return(NULL))
}