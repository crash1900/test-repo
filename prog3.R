best <- function(state, outcome)
{
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
  
  
  ## Check that state and outcome are valid
  validState = state %in% unique(outcomeData$State)
  validOutcome <- FALSE
  col <- -1
  
  if (tolower(outcome) == 'heart attack')
  {
    col <- 11
    validOutcome <- TRUE
  }
  else if (tolower(outcome) == 'heart failure')
  {
    col <- 17
    validOutcome <- TRUE
  }
  else if (tolower(outcome) == 'pneumonia')
  {
    col <- 23
    validOutcome <- TRUE
  }
  
  if (!validState)
  {
    stop('invalid state')

  }
  else if (!validOutcome)
  {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Filter the states first
  #statesFiltered <- outcomeData[outcomeData$state == state & outcomeData[,col] != 'Not Available', ]
  store <- outcomeData[outcomeData[,7] == state,]
  store2 <- which.min(store[,col])
  statesFiltered <- subset(outcomeData, outcomeData$state == 'AL')
  statesFiltered
  #test <- which.min(statesFiltered[,col])
  #test
}