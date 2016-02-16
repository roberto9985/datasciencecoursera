## Validates the value of the `rank` parameter.
validateRank <- function(rank){
    (rank %in% c("best", "worst"))||
        is.numeric(rank)
}

rankall <- function(outcome, num = "best") {
    ## Load the utility functions that validate arguments,
    ## filter data by state and provide the index of the outcome column
    source("utils.R")
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome and rank are valid
    if(!validateRank(num))
        stop("invalid rank")
    outcomeIndex <- getOutcomeIndex(outcome)
    if(is.null(outcomeIndex))
        stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    result <- data.frame()
    valuesForEachState <- lapply(split(data,
                                       data[, 7]),
                                 function(stateData){
                                     orderedData <- stateData[order(stateData[, outcomeIndex], stateData[, 2],na.last=NA),]
                                     if(num == "best")
                                         head(orderedData, 1)
                                     else if(num == "worst")
                                         tail(orderedData, 1)
                                     else
                                         orderedData[num, ]
                                 })
    result <- do.call(rbind, valuesForEachState)
    data.frame(hospital = result[, 2], state = result[, 7])
    ## result[, c(7, 2)]                   
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}
