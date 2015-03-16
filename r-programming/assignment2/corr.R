corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  result <- numeric(0)
  
  c <- complete(directory)
  c <- c[c$nobs > threshold, ]
  for (i in c$id) {
    path <- paste(directory, "/", sprintf("%03d.csv", i), sep="")
    data <- read.csv(path)
    result <- c(result, cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs"))
  } 
  
  return(result)
}
