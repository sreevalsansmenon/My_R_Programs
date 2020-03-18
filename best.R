best <- function(state, outcome){
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states = outcomes["State"]
  out = outcomes[outcome]
  if sum(){
    
  }
  ## Return hospital name in that state with lowest 30-day death ## rate
}