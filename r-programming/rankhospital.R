## Ranks the hospital by outcome in a state
rankhospital <- function(state, outcome, num = "best"){
    ## Load the utility functions that validate arguments,
    ## filter data by state and provide the index of the outcome column
    source("utils.R")
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    outcomeIndex <- getOutcomeIndex(outcome)

    ## Check that state and outcome are valid
    if(!isValidState(data, state))
        stop("invalid state")
    outcomeIndex <- getOutcomeIndex(outcome)
    if(is.null(outcomeIndex))
        stop("invalid outcome")
    
    stateData <- getStateData(data, state, outcomeIndex)
    orderedData <- stateData[order(stateData[, outcomeIndex], stateData[, 2]), ]
    rank <- parseRank(num, stateData, outcomeIndex)
    orderedData[rank, 2] 
}
