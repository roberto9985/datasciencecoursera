## Checks if state is valid by verifying if the
## provided value is in the list of states from the data file
isValidState <- function(data, state){
    ## States is the 7th column
    states <- unique(data[, 7])
    state %in% states
}

## Returns the index of the outcome column by checking in a
## hardcoded list of known values. If the outcome is not found returns NULL
getOutcomeIndex <- function(outcome){
    names <- list("heart attack", "heart failure", "pneumonia")
    indices <- list(11, 17, 23)
    frame <- data.frame(indices)
    names(frame) <- names
    frame[[outcome]]
}

## Finds the best hospital in a state
best <- function(state, outcome){
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
    stateData <- subset(data, data[,7] == state)
    ## Coerce the target column to numeric
    stateData[, outcomeIndex] <- as.numeric(stateData[, outcomeIndex])
    ## Calculate the minimum per state
    minPerState <- min(stateData[, outcomeIndex], na.rm = TRUE)
    ## Order state data by hospital name
    stateData <- stateData[order(stateData[, 2]),]
    ## Return the first hospital that has the outcome equal to minimum
    subset(stateData, stateData[, outcomeIndex] == minPerState)[1, 2]
}
