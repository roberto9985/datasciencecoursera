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
    ## Using `suppressWarnings` will suppress the annoying
    ## warning message "In getStateData(data, state, outcomeIndex) : NAs introduced by coercion"
    stateData[, outcomeIndex] <- suppressWarnings(as.numeric(stateData[, outcomeIndex]))
    return(stateData)
}

## Parses the rank of the hospital from the arguments
## Rank can be any number or strings "best", "worst"
parseRank <- function(rank, data, outcomeIndex){
    if(rank == "best") return(1)
    if(rank == "worst"){
        missingValues <- sapply(data[, outcomeIndex], is.na)
        numberOfMissingValues <- sum(missingValues)
        return(length(data[, outcomeIndex]) - numberOfMissingValues)
    }
    if(!is.numeric(rank)){
        stop("invalid rank")
    }
    return(rank)
}
