library(corrplot)
library(e1071)
library(stringr)

variable_analysis <- function(endyear = 2015){
  setwd(paste0("~/Dropbox (LIBR)/Neighborhood Analysis"))
  acs_data2 <- read.csv("fa results/useful_ACS_originalData.csv", row.names = 1)
  
  acs_data <- acs_data[,!grepl("Total", colnames(acs_data))]
  
  # Variance
  acs_data_vari <- matrix()
  acs_data_mean <- matrix()
  for (i in 1:length(acs_data)){
    acs_data_vari[i] <- var(acs_data[,i], na.rm = T)
    acs_data_mean[i] <- mean(acs_data[,i], na.rm = T)
  }
  names(acs_data_vari) <- names(acs_data)
  acs_data_vari <- t(as.matrix(acs_data_vari))
  acs_data_sd <- sqrt(acs_data_vari)
  names(acs_data_mean) <- names(acs_data)
  acs_data_mean <- t(as.matrix(acs_data_mean))
  acs_data_coffov <- acs_data_sd/acs_data_mean
  names(acs_data_coffov) <- names(acs_data)
  acs_data_coffov <- as.matrix(acs_data_coffov[order(acs_data_coffov)])
  
  barCol <- ifelse(grepl("Total..", colnames(acs_data_vari)),"grey85","grey50")
  
  png(paste0("fa results/ACS_coefficientofvariance.png"), width = 2000, height = 3000)
  par(las=2)
  par(mar=c(40,4,1,1))
  barplot(acs_data_coffov, beside = TRUE, col = barCol)
  dev.off()
  
  write.csv(acs_data_coffov, "fa results/ACS_coefficientofvariance.csv")
  
  # Covariance
  corr <- cor(acs_data)
  
  png(paste0("fa results/ACS_correlation.png"), width = 6000, height = 6000)
  corrplot(corr, is.corr=T, method = 'color', tl.cex = 1)
  dev.off()
  
  matches <- matrix(nrow=0, ncol=3)
  for(i in 2:dim(corr)[2]){
    matches <- rbind(matches, cbind(rep(colnames(corr)[i],(i-1)), rownames(corr)[1:(i-1)], corr[c(1:(i-1)),i]))
  }
  
  View(matches[which(matches[,3]>0.9),])
  
  # Histograms
  nvar <- length(acs_data)
  
  nzero <- matrix(nrow = 1, ncol = nvar)
  colnames(nzero) <- colnames(acs_data)
  for (i in 1:nvar){
    nzero[1,i] <- length(which(acs_data[,i] == 0))
  }
  write.csv(nzero, "fa results/nzero.csv")
  
  n.na <- matrix(nrow = 1, ncol = nvar)
  colnames(n.na) <- colnames(acs_data)
  for (i in 1:nvar){
    n.na[1,i] <- length(which(is.na(acs_data[,i])))
  }
  write.csv(n.na, "fa results/nna.csv")
  
  dir.create("results/histograms")
  
  for(i in 1:nvar){
    png(paste0("fa results/histograms_raw/",i, "_", colnames(acs_data)[i], "_hist.png"))
    max_i <- max(acs_data[,i], na.rm = T)
    min_i <- min(acs_data[,i], na.rm = T)
    breakpoints_i <- seq(min_i, max_i, length.out = 50)
    hist(acs_data[,i], main = colnames(acs_data)[i], breaks = breakpoints_i)
    
    kurt_i <- round(kurtosis(acs_data[,i], na.rm = T), 3)
    mtext(paste0('nZero = ', nzero[1,i], ' ; nNA = ', n.na[1,i], '; kutosis = ', kurt_i))
    dev.off()
  }
}