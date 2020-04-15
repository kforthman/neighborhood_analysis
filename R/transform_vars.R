# This function finds the optimal transformation for normalization of each of the variables
# and outputs a matrix of the transformed data.
transform_vars <- function(endyear = 2015, scale = T, keepdim = T){
  filename <- paste0("output/", endyear, "/useful_ACS_transformedData.csv")
  bool_q <- -1
  if (file.exists(filename)){
    while(bool_q != 1){
      bool_q <- readline("Transformed dataset has already been created... Would you like to recreate it? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'.")}
    }
  }
  else{
    while(bool_q != 1){
      bool_q <- readline("Transformed dataset has not been created... Would you like create it? Y/N(1/0)  ")
      if(bool_q == 0){return()}
      else if(bool_q != 1){message("Please enter a 1 or 0 for 'Yes' or 'No'")}
    }
  }
  
  print("Begin transforming variables...")
  
  # Grab the altered ACS matrix
  mydata <- read.csv(paste0("output/", endyear, "/useful_ACS_condensedData.csv"), row.names = 1)
  
  hist_fold_name <- paste0("plots/", endyear,"/histograms_transformedData")
  kurt_fold_name <- paste0("plots/", endyear,"/kurtosis_BY_transformationStrength")
  cond_fold_name <- paste0("plots/", endyear,"/histograms_condensedData")
  trans_mat_name <- paste0("output/", endyear,"/useful_ACS_transformedData.csv")
  
  # Creates directories to store the histograms of the transformed data
  # Deletes the directory and creates a new one if the directory is already there.
  unlink(hist_fold_name, recursive = T)
  unlink(kurt_fold_name, recursive = T)
  unlink(cond_fold_name, recursive = T)
  
  dir.create(hist_fold_name, showWarnings = F)
  dir.create(kurt_fold_name, showWarnings = F)
  dir.create(cond_fold_name, showWarnings = F)
  
  # Define the number of variables.
  n <- dim(mydata)[1]
  nvar <- dim(mydata)[2]
  
  # Create an empty matrix to store the transformed variables.
  trans_data <- matrix(nrow = n, ncol = 0)
  
  # n_trans_val is the number of different strengths of transformation we want to test
  # for getting the most normal curve.
  n_trans_val <- 50
  
  # Data is bent so that is is curved in a particular direction.
  # Right skewed data will be bent so that they curve concave down.
  # Left skewed data will be bent so that they curve concave up.
  for(i in 1:nvar){
    var_name <- names(mydata)[i]
    var_obs <- mydata[,i]
    
    var_obs_skewness <- round(skewness(var_obs, na.rm = T), 1)
    
    print( paste0(i, '_', var_name, ' ... Skewness: ', var_obs_skewness) )
    
    m <- max(var_obs, na.rm = T)
    # The transformation values alter the strength of the transformation. The larger the 
    # trans_val is, the less strong the transformation will be.
    trans_val <- seq(0, m, length.out = n_trans_val+1)
    trans_val <- trans_val[2:(n_trans_val+1)]
    # The trans values are given a logarithmic step scale. This is because
    # smaller values have a larger incremental effect on the transformation
    # strength.
    trans_val <- (m*(exp(5*trans_val/m) - 1)) / (exp(5) - 1)
    
    # Now, the function does one transformation with variables skewed to the right
    # and the opposite transformation for variables skewed to the left.
    if(var_obs_skewness > 1){
      
      # The function that transforms the variables has different effects for
      # different values of the trans_val. For smaller values of trans_val,
      # the transformation function will have a stronger effect on the variable's
      # distribution.
      trans_func <- function(trans_val){
        h <- m/(log(m+trans_val)-log(trans_val))
        var_obs_trans <- h*(log(var_obs + trans_val) - log(trans_val))
        return(var_obs_trans)
      }
      
      # Each column of the following matrix is a transformation of a different strength.
      var_obs_trans_mat <- sapply(trans_val, trans_func)
      colnames(var_obs_trans_mat) <- trans_val #round(trans_val, 1)
      # The kurtosis is found for each strength of transformation.
      kurt <- sapply(seq(1,n_trans_val), function(x){kurtosis(var_obs_trans_mat[,x], na.rm = T)})
      
      # The trans_val that results in the best kurtosis is found
      kurt_min <- min(abs(kurt), na.rm = T)
      kurt_min_loc <- which(abs(kurt) == kurt_min)
      
      # Record the optimal trans val and the resulting kurtosis of its distribution
      mytrans <- trans_val[kurt_min_loc]
      mykurt <- kurt[kurt_min_loc]
      
      # Record the transformed variable.
      var_obs_trans <- as.matrix(var_obs_trans_mat[,kurt_min_loc])
      h <- m/(log(m + mytrans)-log(mytrans))
      mytext <- paste0('right skewed; transformation = ', round(h,3), ' * (log(x + ',
                       round(mytrans,3),
                       ') - log(', round(mytrans,3), ')) ; kurtosis = ',
                       round(mykurt,3))
    }else if(var_obs_skewness < -1){
      
      # This function is the same as the one above, but flipped on the y=x axis.
      trans_func <- function(trans_val){
        h <- m/(log(m+trans_val)-log(trans_val))
        var_obs_trans <-  exp(var_obs/h + log(trans_val)) - trans_val
        return(var_obs_trans)
      }
      
      var_obs_trans_mat <- sapply(trans_val, trans_func)
      colnames(var_obs_trans_mat) <- trans_val #round(trans_val, 1)
      kurt <- sapply(seq(1,n_trans_val), function(x){kurtosis(var_obs_trans_mat[,x], na.rm = T)})
      
      kurt_min <- min(abs(kurt), na.rm = T)
      kurt_min_loc <- which(abs(kurt) == kurt_min)
      
      mytrans <- trans_val[kurt_min_loc]
      mykurt <- kurt[kurt_min_loc]
      
      var_obs_trans <- as.matrix(var_obs_trans_mat[,kurt_min_loc])
      h <- m/(log(m + mytrans)-log(mytrans))
      mytext <- paste0('left skewed; transformation = e^(x/',
                       round(h,3), ' + log(',
                       round(mytrans,3), ')) - ', round(mytrans,3), ' ; kurtosis = ',
                       round(mykurt,3))
    }else{
      
      # If the variable is not that skewed, it is not transformed.
      var_obs_trans <- var_obs
      kurt <- seq(0, 0, length.out = n_trans_val)
      mykurt <- kurtosis(var_obs_trans, na.rm = T)
      mytext <- paste0('normal, no transformation; kurtosis = ', round(mykurt,3))
    }
    
    # If the observations are supposed to be of zero mean and unit variance, they are scaled.
    var_obs_trans <- scale(var_obs_trans)
    
    # # The following is to assure that the breaks are uniform accross variables.
    # max_var_obs_trans <- max(var_obs_trans, na.rm = T)
    # min_var_obs_trans <- min(var_obs_trans, na.rm = T)
    # breakpoints_var_obs_trans <- seq(min_var_obs_trans, max_var_obs_trans, length.out = 50)
    
    # Create histograms for the transformed variables.
    png(paste0(hist_fold_name, "/", i, "_", var_name, "_hist.png"))
    hist(var_obs_trans, main = paste(var_name, "Transformed"), 
         breaks = 50)
    mtext(mytext, cex = 0.9)
    dev.off()
    
    png(paste0(cond_fold_name, "/", i, "_", var_name, "_hist.png"))
    hist(var_obs, main = paste(var_name), 
         breaks = 50)
    #mtext(mytext, cex = 0.9)
    dev.off()
    
    # The kurtosis for each transformation value.
    png(paste0(kurt_fold_name, "/", i, "_", var_name, "_kurt.png"))
    plot(trans_val, kurt, main = var_name)
    dev.off()
    
    # Binds the transformed variable to a new matrix.
    trans_data <- cbind(trans_data, matrix(var_obs_trans))
  }
  
  trans_data <- knnImputation(trans_data)
  
  colnames(trans_data) <- names(mydata)
  rownames(trans_data) <- rownames(mydata)
  write.csv(trans_data, trans_mat_name)
  print("Transfomed data matrix created.")
}