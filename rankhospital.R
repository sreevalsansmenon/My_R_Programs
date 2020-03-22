rankhospital <- function(state, outcome, num = "best"){
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
    min_data = data[!is.na(data_st),]
    min_data[,outcol]=as.numeric(min_data[,outcol])
    min_data_sort1 = min_data[with(min_data, order(min_data[,2])), ]
    min_data_sort2 = min_data_sort1[with(min_data_sort1, order(min_data_sort1[,outcol])), ]
    if (num == "best") {
      min_data_sort2[1,2]
    }
    else if (num == "worst") {
      min_data_sort2[nrow(min_data_sort2),2]
    }
    else
    min_data_sort2[num,2]
  }
  ## Return hospital name in that state with lowest 30 day death ## rate
}
