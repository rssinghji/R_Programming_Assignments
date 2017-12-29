rankall <- function(outcome, num = "best")
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
    if(!(outcome %in% colnames(tableData)))
    {
        stop("invalid outcome")
    }
    
    #Find a distinct state vector
    uniqueStates <- unique(tableData[,"State"])
    uniqueStates <- sort(uniqueStates)
    
    #Clean the table
    tableData[, outcome] <- suppressWarnings(as.numeric(tableData[,outcome]))
    tableData <- tableData[complete.cases(tableData[,outcome]),]
    
    #initialize result
    resultDataFrame <- data.frame(Characters=character(),Characters=character(),
                                  stringsAsFactors=FALSE)
    names(resultDataFrame)<-c("hospital","state")
    
    ## For each state, find the hospital of the given rank
    for(state in uniqueStates)
    {
        stateSpecificTable <- tableData[tableData$State == state,]
        stateSpecificTable[, outcome] <- suppressWarnings(as.numeric(stateSpecificTable[,outcome]))
        stateSpecificTable <- stateSpecificTable[complete.cases(stateSpecificTable[,outcome]),]
        stateSpecificTable <- stateSpecificTable[order(stateSpecificTable[,outcome],
                                                       stateSpecificTable[,1]),]
        if(num == "best")
        {
            tempDataFrame <- data.frame(stateSpecificTable[1,1],state)
            names(tempDataFrame)<-c("hospital","state")
            resultDataFrame <- rbind(resultDataFrame, tempDataFrame)
        }
        else if(num == "worst")
        {
            tempDataFrame <- data.frame(stateSpecificTable[nrow(stateSpecificTable),1],state)
            names(tempDataFrame)<-c("hospital","state")
            resultDataFrame <- rbind(resultDataFrame, tempDataFrame)
        }
        else
        {
            if(num > nrow(stateSpecificTable))
            {
                tempDataFrame <- data.frame("NA",state)
                names(tempDataFrame)<-c("hospital","state")
                resultDataFrame <- rbind(resultDataFrame, tempDataFrame)
            }
            else
            {
                tempDataFrame <- data.frame(stateSpecificTable[num,1],state)
                names(tempDataFrame)<-c("hospital","state")
                resultDataFrame <- rbind(resultDataFrame, tempDataFrame)
            }
        }
    }
    
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    resultDataFrame <- resultDataFrame[order(resultDataFrame[, 2]),]
    return(resultDataFrame)
}