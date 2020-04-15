# Creates 2000 bootstrap samples and then finds the error of the factor model.
fa_bootstrap <- function(nfactors = 5, endyear = 2015){
filename <- paste0("output/", endyear, "/", nfactors, " factors/bootstrap_error.csv")
bool_q <-  -1
if (file.exists(filename)){
  while(bool_q != 1){
    bool_q <- readline("Bootstrap error has already been calculated... Would you like to calculate it again? Y/N(1/0)  ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'.")}
  }
}
else{
  while(bool_q != 1){
    bool_q <- readline("Bootstrap error has not been calculated... Would you like calculated it? Y/N(1/0)  ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
  }
}

  # all_mat has all vars
  all_mat <- read.csv(paste0("output/",endyear,"/useful_ACS_transformedData.csv"), row.names = 1)
  
  all_fa <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
  
  p <- dim(all_fa$loadings)[1]
  
  colnames(all_fa$loadings) <- factorName(colnames(all_fa$loadings))
  
  
  list_bootload <- list()
  
  initTime <- Sys.time()
  for(i in 1:2000){
    my_samp <- sample.int(nrow(all_mat), nrow(all_mat), replace = TRUE)
    
    my_boot <- all_mat[my_samp,]
    
    write.csv(my_boot, paste0("output/",endyear,"/", nfactors," factors/bootstrap/data/bootstrap_", i, ".csv"))
    
    print(paste("Matrix", i, "saved."))
    
    boot_fa <- fa(my_boot, nfactors = nfactors, rotate= "oblimin", fm = 'minres')
    
    print(paste("FA", i, "complete."))
    
    all_boot_cor <- matrix(nrow = nfactors, ncol = nfactors)
    
    for(j in 1:nfactors){
      for(k in 1:nfactors){
        all_boot_cor[j,k] <- cor(all_fa$loadings[,j], boot_fa$loadings[,k])
      }
    }
    
    my.order <- sapply(c(1:nfactors), function(x){which.max(all_boot_cor[,x])})
    
    colnames(boot_fa$loadings) <- colnames(all_fa$loadings)[my.order]
    
    saveRDS(boot_fa, paste0("output/",endyear,"/", nfactors," factors/bootstrap_", i, ".rds"))
    
    
    
    list_bootload[[i]]  <- boot_fa$loadings[,c("Racial Homogeneity", "Senoirity", "Singleton", "Affluence", "Citizenship")]
    
    remainTime <- (difftime(Sys.time(), initTime, units = "secs")/i)*(2000-i)
    print(paste0(i, " | Estimated time remaining: ", round(as.numeric(remainTime)/60/60, 2), 
                 " hours. Estimated end time: ", (Sys.time() + remainTime)))
  }
  
  
  # --------------------------------------------------------------------------------------------------------------
  
  
  sum <- matrix(0, nrow = p, ncol = nfactors)
  for (i in 1:2000){
    sum <- sum + list_bootload[[i]]
  }
  
  mean <- sum/2000
  
  sum2 <- matrix(0, nrow = p, ncol = nfactors)
  for (i in 1:2000){
    sum2 <- sum2 + (list_bootload[[i]] - mean)^2
  }
  
  se <- sqrt(sum2/1999)
  
  write.csv(se, filename)
  
}