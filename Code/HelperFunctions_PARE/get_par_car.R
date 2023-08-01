# Function to get parameter estimate matrix from CAR model fits, given...
#   ln.scale.fit: the CAR fit object for ln.scale
#   shape.fit: the CAR fit object for shape
#   rate.fit: the CAR fit object for rate
get_par_car <- function(ln.scale.fit, shape.fit, rate.fit){
  car_fit <- rbind(exp(summary(ln.scale.fit)$fit$coef),
                   summary(shape.fit)$fit$coef,
                   summary(rate.fit)$fit$coef)
  rownames(car_fit) <- c("scale", "shape", "rate")
  return(car_fit)
}