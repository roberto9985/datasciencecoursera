## Finds the best hospital in a state
best <- function(state, outcome){
    ## Load the utility functions that validate arguments,
    ## filter data by state and provide the index of the outcome column
    source("utils.R")
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!isValidState(data, state))
        stop("invalid state")
    outcomeIndex <- getOutcomeIndex(outcome)
    if(is.null(outcomeIndex))
        stop("invalid outcome")

    ## Return hospital name in that state with lowest 30-day death rate

    ## Filter the data by state
    stateData <- getStateData(data, state, outcomeIndex)
    ## Calculate the minimum per state
    minPerState <- min(stateData[, outcomeIndex], na.rm = TRUE)
    ## Order state data by hospital name
    stateData <- stateData[order(stateData[, 2]),]
    ## Return the first hospital that has the outcome equal to minimum
    subset(stateData, stateData[, outcomeIndex] == minPerState)[1, 2]
}
