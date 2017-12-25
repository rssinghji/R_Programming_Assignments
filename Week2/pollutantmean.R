updateSumAndCount <- function(finalFileName, pollutant, env)
{
    fileData <- read.csv(finalFileName)
    pollutantVector <- fileData[pollutant]
    pollutantVector <- pollutantVector[!is.na(pollutantVector)]
    
    tempSum <- get("sumValue", envir = env)
    tempSum <- tempSum + sum(pollutantVector)
    assign("sumValue", value = tempSum, envir = env)
    
    tempCount <- get("countValue", envir = env)
    tempCount <- tempCount + length(pollutantVector)
    assign("countValue", value = tempCount, envir = env)
}

pollutantmean <- function(directory, pollutant, id = 1:332)
{
    currentDir <- getwd()
    filePath <- paste(currentDir,"/",directory,"/", sep='')
    env <- new.env(parent=emptyenv()) 
    env$sumValue <- 0
    env$countValue <- 0
    
    for (index in id)
    {
        fileName1 <- "00"
        fileName2 <- "0"
        fileName3 <- ""
        if(index < 10)
        {
            fileName1 <- paste(fileName1, index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName1, sep='')
            
            updateSumAndCount(finalFileName, pollutant, env)
        }
        else if(index >= 10 && index < 100)
        {
            fileName2 <- paste(fileName2,index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName2, sep='')
            
            updateSumAndCount(finalFileName, pollutant, env)
        }
        else
        {
            fileName3 <- paste(index, ".csv" ,sep='')
            finalFileName <- paste(filePath, fileName3, sep='')
            
            updateSumAndCount(finalFileName, pollutant, env)
        }
    }
    meanOfPollutants <- env$sumValue/env$countValue
    print(meanOfPollutants)
}