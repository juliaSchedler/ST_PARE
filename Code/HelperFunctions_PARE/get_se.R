# Function to get SE's from CAR model fits
get_se <- function(ln.scale.fit, shape.fit, rate.fit){  # still returns log(scale)...
  mat <- NULL
  ln.scale_vcov <- ln.scale.fit$fit$imat * ln.scale.fit$fit$s2
  mat <- rbind(mat, sqrt(diag(ln.scale_vcov)))
  shape_vcov <- shape.fit$fit$imat * shape.fit$fit$s2
  mat <- rbind(mat, sqrt(diag(shape_vcov)))
  rate_vcov <- rate.fit$fit$imat * rate.fit$fit$s2
  mat <- rbind(mat, sqrt(diag(rate_vcov)))
  rownames(mat) <- c("ln.scale", "shape", "rate")
  colnames(mat) <- c("Reg1", "Reg2", "Reg3")
  return(mat)
}