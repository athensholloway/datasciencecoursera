corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  
  for(i in id) {
    path <- paste(directory, "/", sprintf("%03d.csv", i), sep="")
    data <- read.csv(path)
    nobs <- nrow(data[complete.cases(data),])
    results <- rbind(results, c(i, nobs))
  }
  
  ## Return a numeric vector of correlations
}