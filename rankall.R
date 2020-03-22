rankall <- function(outcome, num = "best"){
  ## Read outcome data
  outcol = sum((outcome == c('pneumonia', 'heart attack', 'heart failure'))*c(23,11,17))
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states=outcomes["State"]
  if (outcol == 0) {
    print("Invalid Outcome")
  } else {
    st = unique(states)
    state = st[with(st, order(st[,1])), ]
    siz = length(state)
    output <- data.frame(matrix(ncol=2, nrow=siz[1]) )
    for (i in 1:siz[1]) {
      data = outcomes[states==state[i],]
      data_st = as.numeric(data[,outcol])
      min_data = data[!is.na(data_st),]
      min_data[,outcol]=as.numeric(min_data[,outcol])
      min_data_sort1 = min_data[with(min_data, order(min_data[,2])), ]
      min_data_sort2 = min_data_sort1[with(min_data_sort1, order(min_data_sort1[,outcol])), ]
      if(num == "best") {
        output[i,1] = min_data_sort2[1,2]
        output[i,2] = state[i]
      } else if (num == "worst") {
        min_data_sort3 = max(min_data_sort2[,outcol])
        data_tp = min_data_sort2[,outcol]
        data_rev = min_data_sort2[data_tp==min_data_sort3,]
        output[i,1] = data_rev[1,2]
        output[i,2] = state[i]
      } else {
       output[i,1] = min_data_sort2[num,2]
       output[i,2] = state[i]
      }
    }
    colnames(output) = c("hospital","state")
    output
  }
  ## Return hospital name in that state with lowest 30 day death ## rate
}
tail(rankall("pneumonia", "worst"), 3)