## Returns the index of the outcome column by checking in a
## hardcoded list of known values. If the outcome is not found returns NULL
getOutcomeIndex <- function(outcome){
    names <- list("heart attack", "heart failure", "pneumonia")
    indices <- list(11, 17, 23)
    frame <- data.frame(indices)
    names(frame) <- names
    frame[[outcome]]
}

## Checks if state is valid by verifying if the
## provided value is in the list of states from the data file
isValidState <- function(data, state){
    ## States is the 7th column
    states <- unique(data[, 7])
    state %in% states
}

## Returns the subset of data specific for the state parameter
getStateData <- function(data, state, outcomeIndex){
    ## State column index is 7
    stateData <- subset(data, data[, 7] == state)
    stateData[, outcomeIndex] <- as.numeric(stateData[, outcomeIndex])
    return(stateData)
}

## Parses the rank of the hospital from the arguments
## Rank can be any number or strings "best", "worst"
parseRank <- function(rank, data, outcomeIndex){
    if(rank == "best") return(1)
    if(rank == "worst"){
        missingValues <- sapply(data[, outcomeIndex], is.na)
        numberOfMissingValues <- sum(missingValues)
        print(numberOfMissingValues)
        return(length(data[, outcomeIndex]) - numberOfMissingValues)
    }
    if(!is.numeric(rank)){
        stop("invalid rank")
    }
    return(rank)
}

## Ranks the hospital by outcome in a state
rankhospital <- function(state, outcome, num = "best"){
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
