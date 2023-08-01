# from Eduardo - function calculating avgs for regions from grid
compute_means <- function(co_krig, regions) {
  num_points <- length(co_krig)
  num_regions <- length(regions)
  ln.scale_avg <- rep(0, num_regions)
  shape_avg <- rep(0, num_regions)
  for (i in 1:num_regions) {
    print(paste0("Outer iter: ", i))
    region <- regions[i, ]
    counter <- 0
    pct_old <- 0
    for (j in 1:num_points) {
      pct <- 100 * j / num_points
      point <- co_krig[j, ]
      if (is.null(rgeos::gDifference(point, region, byid = T))) {
        ln.scale_avg[i] <- ln.scale_avg[i] + co_krig@data$ln.scale.pred[j]
        shape_avg[i] <- shape_avg[i] + co_krig@data$shape.pred[j]
        counter <- counter + 1
      }
      if (pct - pct_old > 5) {
        print(paste0(pct, "% of outer iter completed"))
        pct_old <- pct
      }
    }
    if (counter != 0) {
      ln.scale_avg[i] <- ln.scale_avg[i] / counter
      shape_avg[i] <- shape_avg[i] / counter
    }
  }
  avg_mat <- rbind(ln.scale_avg, shape_avg)
  return(avg_mat)
}
