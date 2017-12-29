rankhospital <- function(state, outcome, num = "best")
{
    #Remove space before proceeding
    outcome <- gsub(" ","",outcome)
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #Extract relevant info for this function
    tableData <- as.data.frame(cbind(outcomeData[,2], outcomeData[,7], outcomeData[,11], 
                                     outcomeData[,17], outcomeData[,23]), stringsAsFactors=FALSE)
    
    #Attach proper column names
    colnames(tableData) <- c("HospitalName","State","heartattack", 
                             "heartfailure", "pneumonia")
    
    ## Check that state and outcome are valid
    #Extract state vector and outcome vector
    stateVector <- tableData[, 2]
    
    if(!(any(stateVector == state)))
    {
        stop("invalid state")
    }
    if(!(outcome %in% colnames(tableData)))
    {
        stop("invalid outcome")
    }

    #Extract the table for corresponding state
    stateSpecificTable <- tableData[tableData$State == state,]
    if(num != "best" && num != "worst")
    {
        if(num > nrow(stateSpecificTable))
        {
            return(NA)
        }
    }

    ## Return hospital name in that state with the given rank ## 30-day death rate
    stateSpecificTable[, outcome] <- suppressWarnings(as.numeric(stateSpecificTable[,outcome]))
    stateSpecificTable <- stateSpecificTable[complete.cases(stateSpecificTable[,outcome]),]
    stateSpecificTable <- stateSpecificTable[order(stateSpecificTable[,outcome], 
                                                   stateSpecificTable[,1]),]
    
    if(num == "best")
        return(stateSpecificTable[1,1])
    else if(num == "worst")
        return(stateSpecificTable[nrow(stateSpecificTable),1])
    else
        return(stateSpecificTable[num,1])
}