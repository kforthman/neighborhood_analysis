# Plots the factor structure for the given model.
plotLoads <- function(nfactors = 5, endyear = 2015){
  
  bool_q <- -1
  while(bool_q != 1){
    bool_q <- readline("Would you like to create the loading plots? Y/N(1/0)  ")
    if(bool_q == 0){return()}
    else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
  }
  
  error <- read.csv(paste0("output/", endyear, "/", nfactors, " factors/bootstrap_error.csv"), row.names = 1)
  # Download the factor model.
  FA_Results <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
  
  # Each element in the following list stores a plot for one of the factors.
  myplots <- list()
  for(i in 1:nfactors){
    p <- length(FA_Results$loadings[,i])
    
    mylabel <- names(FA_Results$loadings[,i])
    labx <- seq(1, p)
    laby <- seq(1.1, 1.1, length.out = p)
    bacy <- seq(1, 1, length.out = p)
    laba <- seq((90 - 360/p/2), by = -360/p, length.out = p) + c(seq(0, 0, length.out = floor(p/2)), seq(180, 180, length.out = ceiling(p/2)))
    labj <-c(seq(0, 0, length.out = floor(p/2)), seq(1, 1, length.out = ceiling(p/2)))
    #colors <- colorNumeric(palette = "Spectral", domain = 13, reverse = T)
    
    
    spoke_length <- c(1,5,1,1,4,3,5,1,1,6,2,6,3)
    
    sll <- length(spoke_length)
    mycolors <- grDevices::rainbow(sll) #colorRampPalette(brewer.pal(11,"Spectral"))(17)
    mycolors <- mycolors[c(1,8,2,9,3,10,4,11,5,12,6,13,7)]
    spoke_x <- c(0.5)
    for(j in 2:sll){
      spoke_x[j] <- spoke_x[j-1] + spoke_length[j-1]
    }
    
    tr <- 0.3
    barAlpha <- ifelse(abs(FA_Results$loadings[,i]) > tr, 1, 0.4)#0.4)
    texAlpha <- ifelse(abs(FA_Results$loadings[,i]) > tr, 1, 0.4)
    barCol <- ifelse(FA_Results$loadings[,i] > 0, "turquoise1", "violetred1")# "midnightblue", "darkred")
    barCol[which(FA_Results$loadings[,i] > 1)] <- "red"
    barCol2 <- rep(c("grey92", "grey94", "grey96"), p)[1:p]
    this_error <- error[,i]
    
    df <- data.frame(var = seq(1,p), load = abs(FA_Results$loadings[,i]) , se = this_error)
    #spoke_colors = mycolors, spoke_length = spoke_length, spoke_x = spoke_x)
    
    ggp <- (ggplot(df) + 
              geom_bar(aes(x = var, y = bacy), stat = "identity", width = 1, fill = barCol2) + 
              geom_bar(aes(x = var, y = load), stat = "identity", width = 1, fill = barCol, 
                       alpha = barAlpha) + 
              geom_errorbar(aes(x = var, ymin = load - se, ymax = load + se), width = 0.5, color = "grey50", size = 2) +
              #geom_rect(aes(xmin = 0.5, xmax = 44.5, ymin = -0.4), ymax = (-0.4 + 0.4*(FA_Results$Vaccounted[2,i])), fill = "orange1") +
              scale_y_continuous(limits = c(-0.4, 1.5)) +
              theme(plot.background = element_rect(fill = 'transparent', color = "transparent"),
                    panel.background = element_rect(fill = 'transparent'),
                    axis.ticks=element_blank(),
                    axis.title=element_blank(),
                    axis.text=element_blank(), panel.grid = element_blank()) + 
              annotate("text", labx, laby, label = mylabel, angle = laba, 
                       size = 30, col = "black", hjust = labj, alpha = texAlpha) +
              annotate("text", 0.5, -0.4, label = paste0(factorName_n(colnames(FA_Results$scores)[i]), "\n(", as.character(100*round(FA_Results$Vaccounted[2,i],2)), "%)"),#paste0(as.character(100*round(FA_Results$Vaccounted[2,i],2)), "%"), 
                       size = 40, col = "black") +
              annotate("segment", x = spoke_x, xend = spoke_x + spoke_length, y = 1.025, yend = 1.025, color = mycolors, size = 10) +
              
              theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), plot.background = element_rect(fill = "white")) +
              
              coord_polar()
    )
    
    myplots[[i]] <- ggp
    
  }
  
  
  # ------- Import dataset and clean it up ------- #
  mega_trans <- as.matrix(read.csv(paste0("output/", endyear, "/useful_ACS_transformedData.csv"), row.names = 1))
  
  # Create a correlation plot
  mega_corr <- cor(mega_trans) # pearson is default.
  mypal <- colorRampPalette(c("violetred1", "white", "turquoise1"))
  
  png(paste0("plots/", endyear, "/corrplot_ACS_transformedData"), 
      width = 540, height = 540, bg = "transparent")
  corrplot(mega_corr, method = "ellipse", bg = "white", col = mypal(100), tl.col = "black", tl.cex = 5, tl.srt = 90, tl.offset = 0, cl.pos="n",
           addgrid.col = "grey95", mar=c(1,1,1,1), p.mat = abs(mega_corr), sig.level = 0.89, insig = "pch", pch = 15, pch.col = "yellow", pch.cex = 15)
  dev.off()
  
  for(i in 1:nfactors){
    png(paste0("plots/", endyear, "/", nfactors," factors/loadingsGraph_", nfactors, 
               "Factors (f", substr(colnames(FA_Results$scores)[i], 3,3),").png"), 
        width = 5000, height = 5000)
    par(mar=c(1,1,1,1))
    plot(myplots[[i]], add = T)
    #mtext("hello", side = 3)
    dev.off()
  }
  
}
