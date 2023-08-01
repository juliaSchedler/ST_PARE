# Function to format data to stations within the regions, given...
#   window_fit: the window list subset to the moving window of choice (e.g. input window[[82]] for the final 40-yr window)
#   reg_polygons: the regions of interest
#   stations: the locations of the stations
format_data <- function(window_fit, stations, reg_polygons, gof_window){
  # Accessing Model Fits and saving the parameter values (scale, shape, rate) for each station
  ws_scale <- NULL
  ws_shape <- NULL
  ws_rate  <- NULL
  j = 1
  for(i in stat_nos){
    fit <- window_fit[[i]] 
    if(!!sum(is.na(fit))){
      ws_scale[j] <- ws_shape[j] <- ws_rate[j]  <- NA
    }
    if(gof_window[i]<0.05 | is.na(gof_window[i])){
      ws_scale[j] <- ws_shape[j] <- ws_rate[j]  <- NA
      
    }
    else{
      ws_scale[j] <- fit$results$par[1]
      ws_shape[j] <- fit$results$par[2]
      ws_rate[j]  <- fit$rate
    }
    j = j+1
  }
  # add new location (long/lat) columns that we will transform to coordinates, as well as parameter values
  stations_df <- stations%>%
    dplyr::mutate(scale = ws_scale, shape = ws_shape, rate = ws_rate, long = LONGITUDE, lat = LATITUDE)
  
  
  ## make it spatial
  stations<- sf::st_as_sf(stations_df, coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84")
  stations <- st_transform(stations, crs = 2278)
  
  region_int <-sf::st_intersection(stations, reg_polygons)
  region_int$REGION <- as.factor(region_int$REGION)
  h <- model.matrix( ~ REGION - 1, data=region_int)
  inds <- as.integer(rownames(h))
  
  stations_sub_df <- stations_df %>%
    dplyr::filter(STAT_NO %in% inds) %>%
    dplyr::filter(!is.na(scale))
  
  #stations_sub <- project_data(stations_sub_df)
  stations_sub <- sf::st_as_sf(stations_sub_df, coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84")
  stations_sub <- st_transform(stations_sub, crs = 2278)
  
  
  ## Have to redo intersect, because now NAs are removed. 
  ## If remove them before the intersect, the station indicies get messed up. So have to do intersect both before and after
  region_int <- sf::st_intersection(stations_sub, reg_polygons)
  region_int$REGION <- as.factor(region_int$REGION) 
  h0 <- model.matrix( ~ REGION - 1, data=region_int)
  
  # Using stations_sub_df and h0 is indicator of regions
  stations_sub_by_reg <- region_int %>% dplyr::mutate(Reg_fac = region_int$REGION, Reg1 = h0[,1], Reg2 = h0[,2], Reg3 = h0[,3])
  sort_dat <- stations_sub_by_reg %>% dplyr::arrange(Reg3, Reg2) # put stations in order of which regions they fall within
  
  return(sort_dat)
}