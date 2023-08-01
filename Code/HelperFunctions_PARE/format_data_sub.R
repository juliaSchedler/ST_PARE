# Function to format data (a stations_sub data frame) to stations within the regions, given...
#   window_fit: the window list subset to the moving window of choice (e.g. input window[[82]] for the final 40-yr window)
#   reg_polygons: the SpatialPolygonsDataFrame for the regions of interest
format_data_sub <- function(window_fit, reg_polygons){
  # Accessing Model Fits and saving the parameter values (scale, shape, rate) for each station
  ws_scale <- NULL
  ws_shape <- NULL
  ws_rate  <- NULL
  j = 1
  for(i in stat_nos){
    fit <- window_fit[[i]] 
    if(is.na(fit)){
      ws_scale[j] <- ws_shape[j] <- ws_rate[j]  <- NA
    }else{
      ws_scale[j] <- fit$results$par[1]
      ws_shape[j] <- fit$results$par[2]
      ws_rate[j]  <- fit$rate
    }
    j = j+1
  }
  # add new location (long/lat) columns that we will transform to coordinates, as well as parameter values
  stations_df <- stations %>%
    dplyr::mutate(scale = ws_scale, shape = ws_shape, rate = ws_rate, long = LONGITUDE, lat = LATITUDE)
  
  stations <- project_data(stations_df)
  region_int <- sp::over(stations, reg_polygons)
  region_int$REGION <- as.factor(region_int$REGION)
  h <- model.matrix( ~ REGION - 1, data=region_int)
  inds <- as.integer(rownames(h))
  
  stations_sub_df <- stations_df %>%
    dplyr::filter(STAT_NO %in% inds) %>%
    dplyr::filter(!is.na(scale))
  
  # stations_sub <- project_data(stations_sub_df)
  
  return(stations_sub_df)
}