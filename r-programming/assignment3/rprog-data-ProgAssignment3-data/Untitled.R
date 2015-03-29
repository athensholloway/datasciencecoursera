best <- function(state, outcome) {
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
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  
  if(outcome == "heart failure"){
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  
  if(outcome == "pneumonia"){
    outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## Return hospital name in that state with lowest 30-day death
  sorted <- data[order(data[,'Hospital.Name']),]
  states <- sorted[sorted$State == state,]
  minIndex <- which.min(as.double(states[,outcome]))

  ## rate
  states[minIndex, 'Hospital.Name']
}