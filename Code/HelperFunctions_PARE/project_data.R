
# Function to project data given...
#     df: a data frame with data and coordinates
project_data <- function(df, proj4){
  coordinates(df)=~long+lat
  proj4string(df) <- "+proj=longlat +datum=WGS84"
  df <- spTransform(df, CRS(proj4)) # projecting data
  is.projected(df)
  return(df)
}