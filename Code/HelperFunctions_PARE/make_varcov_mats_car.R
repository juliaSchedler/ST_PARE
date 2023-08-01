# Function to generate a list of varcov matrices for each region, given...
#   ln.scale.fit: the CAR fit object for ln.scale
#   shape.fit: the CAR fit object for shape
make_varcov_mats_car <- function(ln.scale.fit, shape.fit){
  varcov_list <- list()
  # reg1 <- reg2 <- reg3 <- matrix(data=NA, nrow=2, ncol=2)
  shape_vcov <- shape.fit$fit$imat * shape.fit$fit$s2
  shape.var <- diag(shape_vcov)
  ln.scale_vcov <- ln.scale.fit$fit$imat * ln.scale.fit$fit$s2
  ln.scale.var <- diag(ln.scale_vcov)
  ln.scale.pred <- ln.scale.fit$fit$coefficients
  
  for(i in 1:length(shape.fit$fit$coefficients)){
    mat <- matrix(data=NA, nrow=2, ncol=2)
    mat[1,1] <- exp(ln.scale.pred[i])^2 * ln.scale.var[i] # doing transformation
    mat[2,2] <- shape.var[i]
    mat[1,2] <- mat[2,1] <- 0 # setting cov to 0 since we model separately... a simplifying assumption that will make our CIs wider
    varcov_list[[i]] <- mat
  }
  return(varcov_list)
}