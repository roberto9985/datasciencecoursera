## Returns the index of the outcome column by checking in a
## hardcoded list of known values. If the outcome is not found returns NULL
getOutcomeIndex <- function(outcome){
    names <- list("heart attack", "heart failure", "pneumonia")
    indices <- list(11, 17, 23)
    frame <- data.frame(indices)
    names(frame) <- names
    frame[[outcome]]
}

getStateData <- function(data, state, outcomeIndex){
    ## State column index is 7
    stateData <- subset(data, data[, 7] == state)
    stateData[, outcomeIndex] <- as.numeric(stateData[, outcomeIndex])
    return(stateData)
}

parseRank <- function(rank, data, outcomeIndex){
    if(rank == "best") return(1)
    if(rank == "worst"){
        missingValues <- sapply(data[, outcomeIndex], function(value){
                                    sum(is.na(value))
                                })
        length(data[, outcomeIndex]) - missingValues
    }
    if(!is.numeric(rank)){
        stop("invalid rank")
    }
    return(rank)
}

rankhospital <- function(state, outcome, num = "best"){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## TODO: Validate state

    outcomeIndex <- getOutcomeIndex(outcome)
    stateData <- getStateData(data, state, outcomeIndex)
    orderedData <- stateData[order(stateData[, c(outcomeIndex, 2)]), ]
    orderedData[seq(1, parseRank(num, data, outcomeIndex)), outcomeIndex]
}
