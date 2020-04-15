# This function saves all useful ACS files
saveAllUseful <- function(endyear = 2015){
  bool_q <-  -1
  filename <- paste0("data/ACS_data/", as.character(endyear))
  # If the ACS files have already been downloaded, the function will not proceed.
  if (file.exists(paste0(filename,"/WY/B98012.rds"))){
    print("ACS files have already been downloaded.")
    return()
  }
  while(bool_q != 1){
    bool_q <- readline("ACS files have not yet been downloaded... would you like to download them now? Y/N(1/0)   ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){print("Please enter a 1 or 0 for 'Yes' or 'No'.")}
  }
  
  dir.create(filename)
  
  # Read in the table of useful acs names, created previously. The function will use this list to choose which tables to download.
  useful_ACS_tables <- substr(useful_ACS_tables[,1], 1, 6)
  
  for(k in 1:length(state.codes)){
    
    # Create the directory which will store the ACS variables 
    dir.create(paste0(filename, "/", state.codes[k]))
    
    # Save all useful ACS files as rds files in the created directory.
    options(warn = -1)
    for(i in 1:length(useful_ACS_tables)){
      tracts <- geo.make(state = state.codes[k], county = "*", tract = "*")
      data <- acs.fetch(endyear = endyear, geography = tracts, table.number = useful_ACS_tables[i])
      saveRDS(data, paste0(filename, "/", state.codes[k], "/", useful_ACS_tables[i], ".rds"))
      print(paste0(i, " in ", length(useful_ACS_tables), " ACS files downloaded for ", state.codes[k], "."))
    }
  }
  
  options(warn = 0)
}
