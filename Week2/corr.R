updateCorrelationVector <- function(finalFileName, threshold, env)
{
    fileData <- read.csv(finalFileName)
    fileData <- fileData[complete.cases(fileData), ]
    numberOfObs <- nrow(fileData)
    
    if(numberOfObs > threshold)
    {
        sulphateVector <- fileData["sulfate"]
        nitrateVector <- fileData["nitrate"]
        correlationResult <- cor(sulphateVector,nitrateVector)
        tempVector <- get("correlationVector", envir = env)
        tempVector <- append(tempVector, c(correlationResult))
        assign("correlationVector", value = tempVector, envir = env)
    }
}

corr <- function(directory, threshold = 0)
{
    currentDir <- getwd()
    filePath <- paste(currentDir,"/",directory,"/", sep='')
    
    env <- new.env(parent=emptyenv()) 
    env$correlationVector <- c()
    
    for (index in 1:332)
    {
        fileName1 <- "00"
        fileName2 <- "0"
        fileName3 <- ""
        if(index < 10)
        {
            fileName1 <- paste(fileName1, index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName1, sep='')
            
            updateCorrelationVector(finalFileName, threshold, env)
        }
        else if(index >= 10 && index < 100)
        {
            fileName2 <- paste(fileName2,index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName2, sep='')
            
            updateCorrelationVector(finalFileName, threshold, env)
        }
        else
        {
            fileName3 <- paste(index, ".csv" ,sep='')
            finalFileName <- paste(filePath, fileName3, sep='')
            
            updateCorrelationVector(finalFileName, threshold, env)
        }
    }
    result <- env$correlationVector
}