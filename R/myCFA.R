# Performs the confirmatory factor analysis.
myCFA <- function(nfactors = 5, endyear = 2015){
  filename <- paste0("output/",endyear,"/", nfactors," factors/CFA_", nfactors, "Factors.rds")
  bool_q <- -1
  if (file.exists(filename)){
    while(bool_q != 1){
      bool_q <- readline("Confirmatory factor analysis has already been performed... Would you like to perform the confirmatory factor analysis again? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  else{
    while(bool_q != 1){
      bool_q <- readline("Confirmatory factor analysis has not yet been performed... Would you like do it now? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
    }
  }
  
  FA_Results <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
  FA_load <- as.matrix(FA_Results$loadings)
  useful_ACS_transformedData <- read.csv(paste0("output/", endyear, "/useful_ACS_transformedData.csv"))
  
  mod_list <- list()
  for(i in 1:nfactors){
    mod_list[[i]] <- paste(factorName(colnames(FA_load)[i]), " =~ ", paste(names(FA_load[which(FA_load[,i]>.3),i]), collapse = " + "))
  }
  my.model <- paste(mod_list, collapse = "\n")
  print(my.model)

  my.cfa <- cfa(my.model, data = useful_ACS_transformedData)
  # Save results
  
  saveRDS(my.cfa, file = filename)
  
  print("Confirmatory Factor model saved.")
}