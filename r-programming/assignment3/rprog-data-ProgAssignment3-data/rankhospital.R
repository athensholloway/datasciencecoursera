rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## Check that state and outcome are valid
  if(!(state %in% data[,'State'])) {
    stop("invalid state")
  }
  
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  if(outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  
  if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  
  if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data <- na.omit(data[c('Hospital.Name', outcome, 'State')])
  data <- data[data$State == state,]
  data <- data[order(as.double(data[,2]), data[,1]),]
  
  rows <- nrow(data)

  result <- NA
  
  if (num == "best") {
    result <- data[1, 1]
  }
  if (num == "worst") {
    result <- data[rows, 1]
  }
  if (is.numeric(num) && num <= rows) {
    result <- data[num, 1]
  }
  
  ## 30-day death rate
  result
}
##rankhospital("TX", "heart failure", 4)
##rankhospital("MD", "heart attack", "worst")
##rankhospital("MN", "heart attack", 5000)