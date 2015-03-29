rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- na.omit(data[c('Hospital.Name', outcome, 'State')])
  data <- data[order(as.double(data[,2]), data[,1]),]
  states <- unique(data['State'])
  states <- states[order(states[1]), ]
  totalStates <- length(states)
  hospital <- vector(length=totalStates)
  
  for(i in 1:totalStates) {
    x <- data[data$State == states[i], ]
    rows <- nrow(x)
    
    if (num == "best") {
      hospital[i] <- x[1, 1]
    }
    if (num == "worst") {
      hospital[i] <- x[rows, 1]
    }
    if (is.numeric(num) && num <= rows) {
      hospital[i] <- x[num, 1]
    }
    if (is.numeric(num) && num > rows) {
      hospital[i] <- NA
    }
  }
  data.frame(hospital=hospital, state=states)
}
##head(rankall("heart attack", 20), 10)
##tail(rankall("pneumonia", "worst"), 3)
##tail(rankall("heart failure"), 10)