# Compares the factor scores in each neighborhood to the rates of poor mental health in those neighborhoods.
MentalHealthCorr <- function(nfactors = 5, endyear = 2015){
  
  bool_q <- -1
  while(bool_q != 1){
    bool_q <- readline("Would you like to create a plot showing the relationship between the factors and mental health rates? Y/N(1/0)  ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
  }
  
  cdc_data <- read.csv("data/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format_.csv")
  
  cdc_data[,4] <- str_pad(as.character(cdc_data[,4]), 11, "left", pad = 0)
  
  t1 <- as.data.frame(cdc_data[,c(4, 49)])
  
  FA_Results <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
  factor_scores <- cbind(rownames(FA_Results$scores), FA_Results$scores)
  factor_scores[,1] <- str_pad(as.character(factor_scores[,1]), 11, "left", pad = 0)
  t2 <- as.data.frame(factor_scores)
  
  merged <- as.matrix(merge(t1,t2, by = 1))
  mycolors <- rep(grDevices::rainbow(10), ceiling(length(stateCodes)/10))
  rf <- colorRampPalette(brewer.pal(9,'YlOrRd'))
  
  fitBYstate <-stateCodes
  
  R2.all.all <- matrix(nrow = 51, ncol = 0)
  # Smooth spline
  for(j in 3:(nfactors+2)){
    R2.all <- matrix(nrow = 0, ncol = 1)
    ss.all <- list()
    col.all <- matrix(nrow = 0, ncol = 1)
    
    x <- as.numeric(merged[,j]) # factor score
    y <- as.numeric(merged[,2]) # MH score
    
    png(paste0("plots/",endyear,"/", nfactors," factors/MH_Corr X PRINT ", colnames(merged)[j], "_spline.png"),
        width = 2.2, height = 2.2, units = "in", res = 500)
    par(bg = "white", col.axis = "black", col.lab = "black", col.main = "black", 
        col.sub = "black", fg = "black", mar = c(2,2.5,1,1), ps = 8*(72/72.272), tcl = 0.1, mgp=c(1,0,0))
    plot(x, y, 
         xlab = factorName(colnames(merged)[j]),
         ylab = "Mental health not good for ≥ 14\ndays among adults aged ≥ 18 years", 
         col = alpha("grey50", 0.2), pch = 16, cex = 0.25)
    
    for(k in 1:length(stateCodes)){
      this.State <- merged[which(grepl(stateCodes[k], substr(merged[,1],1,2))),]
      
      this.x <- as.numeric(this.State[,j]) # factor score
      this.y <- as.numeric(this.State[,2]) # MH score
      
      ss <- smooth.spline(this.x, this.y, df = 3, keep.data = T)
      
      TSS <- sum((this.y - mean(this.y))^2)
      RSS <- sum((this.y - predict(ss, this.x)$y)^2)
      R2 <- 1-(RSS/TSS)
      
      R2.all <- rbind(R2.all, R2)
      col.all <- rbind(col.all, rf(100)[100 - (round(R2,2)*100)])
      ss.all[[length(ss.all)+1]] <- ss
    }
    
    
    for(k in order(R2.all, decreasing = F)){
      lines(ss.all[[k]], type = "l", col = col.all[k], lwd = 0.5)
    }
    
    fitBYstate <- cbind(fitBYstate, R2.all)
    colnames(fitBYstate)[j-1] <- factorName(colnames(merged)[j])
    
    dev.off()
    
    R2.all.all <- cbind(R2.all.all, R2.all)
    colnames(R2.all.all)[j-2] <- factorName(colnames(merged)[j])
  }
  
  legend_image <- as.raster(matrix(rf(20), ncol=1))
  png(paste0("plots/",endyear,"/", nfactors," factors/MH_Corr X PRINT legend.png"),
      width = 0.60, height = 2.2, units = "in", res = 500)
  par(bg = "white", col.axis = "black", col.lab = "black", col.main = "black", 
      col.sub = "black", fg = "black", mar = c(0,0,1,0), ps = 8*(72/72.272), tcl = 0.1, mgp=c(1,0,0))
  plot(c(0,1),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  text(x=0.3, y = seq(.9,.1,l=11), labels = seq(1, 0, l=11), adj = c(0, 0.5))
  text(x = 0, y = 1, labels = expression(R^2), font = 2, adj = c(0, 0.5))
  rasterImage(legend_image, 0, .1, .2,.9)
  rect(0, .1, .2,.9, lwd = 1)
  segments(0, seq(.9,.1,l=11), x1 =0.23, lwd = 1)
  dev.off()
  
  png(paste0("plots/",endyear,"/", nfactors," factors/MH_Corr X PRINT R2_Boxplot.png"), width = 4.5, height = 5, 
      units = "in", res = 500)
  par(mar = c(5,4,1,1), ps = 10, mgp = c(2.2,1,0), yaxs = "i")
  plot.new()
  plot.window(ylim = c(0, 1), xlim = c(0.5,5.5))
  rasterImage(legend_image, 0, 0, 6,1)
  rect(0, 0, 0.6, 1, col = "white", border = "white")
  rect(1.4, 0,1.6, 1, col = "white", border = "white")
  rect(2.4, 0,2.6, 1, col = "white", border = "white")
  rect(3.4, 0,3.6, 1, col = "white", border = "white")
  rect(4.4, 0,4.6, 1, col = "white", border = "white")
  rect(5.4, 0,6, 1, col = "white", border = "white")
  
  rect(0.6, 0, 1.4, quantile(R2.all.all[,1])[2], col = "white", border = "white")
  rect(1.6, 0, 2.4, quantile(R2.all.all[,2])[2], col = "white", border = "white")
  rect(2.6, 0, 3.4, quantile(R2.all.all[,3])[2], col = "white", border = "white")
  rect(3.6, 0, 4.4, quantile(R2.all.all[,4])[2], col = "white", border = "white")
  rect(4.6, 0, 5.4, quantile(R2.all.all[,5])[2], col = "white", border = "white")
  
  rect(0.6, quantile(R2.all.all[,1])[4], 1.4, 1, col = "white", border = "white")
  rect(1.6, quantile(R2.all.all[,2])[4], 2.4, 1, col = "white", border = "white")
  rect(2.6, quantile(R2.all.all[,3])[4], 3.4, 1, col = "white", border = "white")
  rect(3.6, quantile(R2.all.all[,4])[4], 4.4, 1, col = "white", border = "white")
  rect(4.6, quantile(R2.all.all[,5])[4], 5.4, 1, col = "white", border = "white")
  
  boxplot(R2.all.all, ylab = "R^2 value", las = 2, ylim = c(0,1), names = NA, add = T, col = "transparent", border = "black")
  labels <- colnames(R2.all.all)
  text(c(1:5), -0.05, labels = labels, srt = 25, 
       adj = c(1,1), xpd = TRUE, cex=.9)
  dev.off()
  
  colnames(fitBYstate)[1] <- "State"
  write.csv(fitBYstate, paste0("output/", endyear, "/", nfactors, " factors/MH_splineFit.csv"), row.names = F)
  
}