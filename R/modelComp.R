# The following function compares FA results according to the provided
# nfactors and suffix.
modelComp <- function(endyear = 2015){
  
  setwd(paste0("~/Dropbox (LIBR)/Neighborhood Analysis/fa results"))
  
  ACS_data <- read.csv("useful_ACS_transformedData.csv", row.names = 1)
  
  # Define the number of independent variables (tracts) and the number of dependent variables.
  n <- dim(ACS_data)[1] # independent
  p <- dim(ACS_data)[2] # dependent
  
  ACS_data_fa <- list()
  for(i in 1:12){
    ACS_data_fa[[i]] <- fa(ACS_data, rotate = 'oblimin', nfactors = i, fm = 'minres')
  }
  
  # R is the original covariance matrix, and R* is the residual matrix (equal to R minus  the factor loading matrix times its transpose).  
  # r and r* represent the elements of R and R* respectivly.
  
  
  creComp <- function(myList, suffix){
    png(paste0("comp_", suffix,".png"), width = 500, height = 500)
    
    dist <- max(myList) - min(myList)
    if (dist > 0){
      labshift <- dist* 0.5  
      ymin <- min(myList) - labshift*2
      ymax <- max(myList) + labshift*2
    } else {
      labshift <- 0.05
      ymin <- min(myList) - 0.5 - labshift*2
      ymax <- max(myList) + 0.5 + labshift*2
    }
    barplot(myList, 
            ylim = c(ymin, ymax), 
            main = suffix, 
            space = 0.5, 
            col = randomColor(1, luminosity = "bright"), 
            border = F)
    text(as.character(seq(1, by = 1, length.out = 12)), 
         x =seq(1, by = 1.5, length.out = 12),
         y = myList + labshift*0.1, 
         col = "black")
    
    dev.off()
  }
  
  # How well does the factor model reproduce the correlation matrix? This is just (Sigma(r^2) - Sigma(r*^2)) / Sigma(r^2)
  # This is similar to the residual correlation?
  fits <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$fit})
  creComp(fits, "fit")
  
  # How well are the off-diagonal elements reproduced?
  fit.offs <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$fit.off})
  creComp(fit.offs, "fitoff")
  
  # Degrees of freedom. This is the number of observed correlations minus then number of independent parameters.
  dofs <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$dof})
  creComp(dofs, "dof")
  
  # Chi squared of model. Found by examining the size of the residuls compared to their standard error.
  chis <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$chi})
  creComp(chis, "chi")
  
  # This is the value of the function which is minimized by the maximum liklihood procedures. This is reported 
  # for comparison purposes and as a way to estimate chi squared goodness of fit.
  objectives <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$objective})
  creComp(objectives, "obj")
  
  # RMS is the sum of the squared residuals divided by the degrees of freedom.
  rmss <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$rms})
  creComp(rmss, "rms")
  
  # RMSEA
  rmseas <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$RMSEA})
  creComp(rmseas, "rmsea")
  
  # Variance Explained
  varEx <- sapply(seq(1,12), function(x){sum(ACS_data_fa[[x]]$Vaccounted[2,])})
  varCu <- sapply(seq(1,12), function(x){ACS_data_fa[[x]]$Vaccounted[2, paste0("MR", x)]})
  creComp(varEx, "varEX")
  
  # Scree plot
  {png(paste0("scree.png"), width = 3, height = 3, units = "in", res = 500)
    par(ps = 8, mar = c(2,2,1,1), tck = .01, mgp = c(0.75,0,0))
    plot(seq(1,12), ACS_data_fa[[1]]$e.values[1:12], type = "o", pch = 16, col = "red4", ylim = c(0,10), 
         xlab = "Factor Number", ylab = "Eigenvalue", main = "Scree Plot", cex = 1)
    abline(1, 0, lty = 2)
    # lines(5, ACS_data_fa[[1]]$e.values[5], pch = 16, type = "p", col = "red4", cex = 1)
    dev.off()
  }
  
  all <- rbind(chis, dofs, fit.offs, fits, objectives, rmss, rmseas[1,], varCu, varEx)
  rownames(all) <- c("Chi Squared", "Degrees of Freedom", "Fit (Off)", "Fit", "Objective", 
                     "RMS", "RMSEA", "Cumulative Variance", "Variance Explained")
  colnames(all) <- seq(1, dim(all)[2])
  
  write.csv(t(all), "nfactor_comp.csv")
  
  VSS(ACS_data, rotate = "oblimin", fm = 'minres')
}