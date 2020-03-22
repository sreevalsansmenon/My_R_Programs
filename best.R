best <- function(state, outcome){
  ## Read outcome data
  outcol = sum((outcome == c('pneumonia', 'heart attack', 'heart failure'))*c(23,11,17))
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states=outcomes["State"]
  if (sum(states==state)==0) {
    print("Invalid State")
  }
  else if (outcol == 0) {
    print("Invalid Outcome")
  }
  else {
    data = outcomes[states==state,]
    data_st = as.numeric(data[,outcol])
    min_data = min(data_st[!is.na(data_st)])
    Hospital = sort(data[(data_st==min_data),2])
    Hospital[1]
  }
  ## Return hospital name in that state with lowest 30 day death ## rate
}
