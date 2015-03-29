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
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  
  if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  
  if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## Return hospital name in that state with lowest 30-day death
  data <- na.omit(data[c('Hospital.Name', outcome, 'State')])
  data <- data[data$State == state,]
  data <- data[order(as.double(data[,2]), data[,1]),]
  
  ## rate
  data[1, 1]
}
##best("TX", "heart attack")
##best("TX", "heart failure")
##best("MD", "heart attack")
##best("MD", "pneumonia")
##best("BB", "heart attack")
##best("NY", "hert attack")
