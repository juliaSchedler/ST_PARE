# Now going to do GOF for ALL stations, for their fits to the entire station length

data_no_na <- function(data, thresh){
  dat <- list() #list of data for each
  
  for(i in 1:numstat){
    column <- 1+i # add 1 since first column is the date
    station_i <- na.omit(data[, column])
    if(class(try(extRemes::fevd(station_i, threshold=thresh, type="GP", method="MLE")))=="try-error"){ 
      dat[[i]] <- NA
    }else{
      dat[[i]] <- station_i
    }
  }  
  return(dat)
}

