## Validates the value of the `rank` parameter.
validateRank <- function(rank){
    (rank %in% c("best", "worst"))||
        is.numeric(rank)
}

## Get the hospital ranked with the value of `rank` parameter from
## all the hospitals of one state.
getHospitalForState <- function(stateHospitals, outcomeIndex, rank){
    sorted <- order(stateHospitals[, outcomeIndex], stateHospitals[, 2], na.last = NA)
    orderedData <- stateHospitals[sorted, ]
    
    if(rank == "best")
        head(orderedData, 1)
    else if(rank == "worst")
        tail(orderedData, 1)
    else
        orderedData[rank, ]
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

    ## Coerce the `outcomeIndex` column to numeric
    data[, outcomeIndex] <- suppressWarnings(as.numeric(data[, outcomeIndex]))
    
    result <- data.frame()
    ## Group data by state
    groupedByState <- split(data, data[, 7])
    
    ## For each state, find the hospital of the given rank
    valuesForEachState <- lapply(groupedByState, getHospitalForState, outcomeIndex, rank = num)
    result <- do.call(rbind, valuesForEachState)

    ## Rename the output columns for readability
    colnames(result)[2] <- "hospital"
    colnames(result)[7] <- "state"

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result[, c(2, 7)]
}
