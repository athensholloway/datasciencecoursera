pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  means <- c()
  for(monitor in id){
    path <- paste(directory, "/", sprintf("%03d.csv", monitor), sep="")
    data <- read.csv(path)
    na <- is.na(data[, pollutant])
    means <- c(means, data[,pollutant][!na])
  }
  return (round(mean(means), digits=3))
}