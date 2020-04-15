# Create matrix which concatinates all of the decidely useful variable tables and names them appropriately.
createOrgMega <- function(endyear = 2015){
  filename <- paste0("output/", endyear, "/useful_ACS_originalData.csv")
  bool_q <-  -1
  # If the concatenated acs matrix has already been created, the function will not proceed.
  if (file.exists(filename)){
    while(bool_q != 1){
      bool_q <- readline("Original matrix already created... Would you like to recreate it? Y/N(1/0)")
      if(bool_q == 0){return()}
      else if(bool_q != 1){print("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  else{
    while(bool_q != 1){
      bool_q <- readline("Original matrix has not yet been created... would you like to create it now? Y/N(1/0)   ")
      if(bool_q == 0){return()}else if(bool_q != 1){print("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  # Read in the table of useful acs table codes.
  useful_ACS_tables <- as.matrix(useful_ACS_tables)
  useful_ACS_tables[,2] <- substr(useful_ACS_tables[,1], 9, nchar(useful_ACS_tables[,1]))
  useful_ACS_tables[,1] <- substr(useful_ACS_tables[,1], 1, 6)
  
  # Read in the table of useful acs variables residing in each table.
  useful_ACS_variables <- read.csv("data/useful_ACS_variables.csv", header = T)[,2:3]
  useful_ACS_variables <- useful_ACS_variables[which(substr(useful_ACS_variables[,1], 11, 11) == "E"),] # Removes the error terms
  useful_ACS_variables[,1] <- substr(useful_ACS_variables[,1], 1, 10)
  useful_ACS_variables[,2] <- as.character(useful_ACS_variables[,2])
  
  ACS_data <- matrix(ncol = dim(useful_ACS_variables)[1], nrow = 0)
  colnames(ACS_data) <- useful_ACS_variables[,1]
  for(k in 1:length(state.codes)){
    # Start the matrix by setting it equal to the first table.
    B01001 <- readUseful(state.codes[k], endyear, "B01001")
    ACS_data_thisState <- as.data.frame(B01001@estimate)
    
    # Set row names of the matrix to the GEOID
    rownames(ACS_data_thisState) <- paste0(str_pad(B01001@geography$state, 2, "left", pad="0"), 
                                           str_pad(B01001@geography$county, 3, "left", pad="0"), 
                                           str_pad(B01001@geography$tract, 6, "left", pad="0"))
    
    for (i in 2:dim(useful_ACS_tables)[1]){
      # Grab the next table
      theTable <- readUseful(state.codes[k], endyear, useful_ACS_tables[i,1])
      
      # Concatenate the contents of the table with the ACS_data matrix
      ACS_data_thisState <- cbind(ACS_data_thisState, theTable@estimate)
    }
    ACS_data <- rbind(ACS_data, ACS_data_thisState)
  }
  
  
  
  # Lists all of the totals in the detailed list
  totals <- which(useful_ACS_variables[,2] == "Total:")
  # Finds the code for the table of each total
  totalstab <- substr(useful_ACS_variables[totals, 1], 1, 6)
  # Find the name of that table and re-lable totals
  useful_ACS_variables[totals, 2] <- paste0("Total: ", useful_ACS_tables[which(is.element(useful_ACS_tables[,1], totalstab)), 2])
  
  # Change column names to be equivalent to the names in the detailed list.
  colnames(ACS_data) <- useful_ACS_variables[,2]
  
  # Save the concatenated matrix.
  write.csv(ACS_data, filename)
}
