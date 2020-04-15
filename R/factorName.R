factorName <- function(factor, n = 'T'){
  factor_n <- as.numeric(substr(factor, 3,3)) 
  if(length(factor_n) <= 5){
  if(n){
  c("African\nAmericans\nin Tract", 
    "Seniors\nin Tract", 
    "Singletons\nin Tract", 
    "Affluence", 
    "Noncitizens\nin Tract")[factor_n]
  }
  else{
    c("African Americans in Tract", 
      "Seniors in Tract", 
      "Singletons in Tract", 
      "Affluence", 
      "Noncitizens in Tract")[factor_n]
  }
  }
  else{
    paste("Factor", factor_n)
  }
}
