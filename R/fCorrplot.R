# Creates a correlation plot of the factors found in the factor analysis.
fCorrplot <- function(nfactors = 5, endyear = 2015){
  
  bool_q <- -1
    while(bool_q != 1){
      bool_q <- readline("Would you like to create a plot correlating the factors? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
    }
  
  FA_Results <- readRDS(paste0("output/",endyear,"/", nfactors," factors/FA_", nfactors, "Factors.rds"))
  
  par(oma = c(0,0,2,0))
  colnames(FA_Results$scores) <- factorName_n(colnames(FA_Results$scores))
  rownames(FA_Results$score.cor) <- colnames(FA_Results$scores)
  colnames(FA_Results$score.cor) <- colnames(FA_Results$scores)
  png(paste0("plots/",endyear,"/", nfactors," factors/factor_corrplot_unitweighted.png"))
  corrplot(round(FA_Results$score.cor,2), method = "ellipse", type = "upper", 
           main = "Unit Weighted Factor Correlation", mar = c(0,0,2,0),
           order="AOE", bg = NA, tl.col = "black")
  corrplot(FA_Results$score.cor, method = "number", type = "upper", add = T,
           main = "", mar = c(0,0,2,0), col = "black", order="AOE",
           diag=FALSE,tl.pos="n", cl.pos="n", bg = NA)
  dev.off()
  png(paste0("plots/",endyear,"/", nfactors," factors/factor_corrplot.png"), width = 3, height = 3, units = "in", res = 500)
  par(ps = 5)
  #corrplot(FA_Results$r.scores, method = "ellipse", type = "upper", main = "Score Correlation", mar = c(0,0,2,0))
  corrplot(cor(FA_Results$scores, FA_Results$scores), method = "ellipse", 
           type = "upper", main = "", mar = c(0,0,0,0),
           order="AOE", tl.col = "black")
  corrplot(cor(FA_Results$scores, FA_Results$scores), method = "number",  
           type = "upper", add = T, main = "", mar = c(0,0,0,0), col = "black",
           order="AOE", diag=FALSE,tl.pos="n", cl.pos="n", bg = NA)
  dev.off()
  
  for (i in 1:5){
    barCol <- ifelse(FA_Results$loadings[,i] > 0, "turquoise1", "violetred1")
    barplot(abs(FA_Results$loadings[,i]), horiz = F, col = barCol, las = 2, main = colnames(FA_Results$scores)[i])
  }
  
  for (i in 1:5){
    hist(FA_Results$scores[,i], main = colnames(FA_Results$scores)[i])
  }
}