# Performs the factor analysis on the transformed data.
factorAnalysis <- function(nfactors = 5, endyear = 2015){
  filename <- paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds")
  bool_q <- -1
  if (file.exists(filename)){
    while(bool_q != 1){
      bool_q <- readline("Factor analysis has already been performed... Would you like to perform the factor analysis again? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  else{
    while(bool_q != 1){
      bool_q <- readline("Factor analysis has not yet been performed... Would you like do it now? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
    }
  }
  
  
  print("Begin factor analysis")
  
  # ------- Import dataset and clean it up ------- #
  mega_trans <- as.matrix(read.csv(paste0("output/",endyear,"/useful_ACS_transformedData.csv"), row.names = 1))
  
  # Define the number of tracts and the number of variables.
  n <- dim(mega_trans)[1]
  p <- dim(mega_trans)[2]
  
  print("Beginning maximum likelihood factor analysis...")
  # ------- Maximum Likelihood Factor Analysis ------- #
  # entering data and extracting factors, 
  # with varimax rotation 
  
  # Create fit using PSYCH package
  fit <- fa(mega_trans, nfactors = nfactors, rotate="oblimin", fm = 'minres')
  print(fit, digits=2, cutoff=.3, sort=TRUE)
  
  # Save results
  saveRDS(fit, file = filename)
  print("Factor model saved.")
}