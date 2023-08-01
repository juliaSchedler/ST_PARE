# GPD Fit function
# Given a matrix of data, fits a GPD to each column of data and saves the fit objects in a list
fitgpdR <- function(data, thresh){
  gpdfits <- list() #list of gpd fits
  
  for(i in 1:numstat){
    column <- 1+i # add 1 since first column is the date
    station_i <- na.omit(data[, column])
    if(class(try(extRemes::fevd(station_i, threshold=thresh, type="GP", method="MLE")))=="try-error"){ 
      gpdfits[[i]] <- NA
    }else{
      gpdfits[[i]] <- extRemes::fevd(station_i, threshold=thresh, type="GP", method="MLE")
    }
  }  
  return(gpdfits)
}