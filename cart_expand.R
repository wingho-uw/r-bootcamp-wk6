cart_expand <- function (..., frame=TRUE, names=NULL){
  
  # convert arguments to list
  data <- list(...)
  
  # extract number of arguments
  # and the length of each individual list
  n <- length(data)
  m_list <- lapply(data, length)
  
  for (i in seq(1, n)){
    for (j in seq(1, n)){
      
      # Expand the lists that are not current being stepped through
      if (i < j){
        data[[j]] <- rep(data[[j]], each=m_list[[i]])
      } else if (i > j){
        data[[j]] <- rep(data[[j]], times=m_list[[i]])
      }
      
    }
  }
  
  if (frame){ # conversion to data.frame
    data <- as.data.frame(do.call(cbind, data))
    if (!is.null(names)){ # assign names to column name
      colnames(data) <- names
    }
  } else { # no conversion
    if (!is.null(names)){ # assign names to list
      names(data) <- names
    }
  }
  
  data
}