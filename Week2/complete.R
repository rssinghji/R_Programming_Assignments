updateVectors <- function(finalFileName, index, env)
{
    fileData <- read.csv(finalFileName)
    fileData <- fileData[complete.cases(fileData), ]
    numberOfObs <- nrow(fileData)
    
    tempVector <- get("completeCasesVector", envir = env)
    tempVector <- append(tempVector, c(numberOfObs))
    assign("completeCasesVector", value = tempVector, envir = env)
    
    tempCount <- get("fileVector", envir = env)
    tempCount <- append(tempCount, c(index))
    assign("fileVector", value = tempCount, envir = env)
}

complete <- function(directory, id = 1:332)
{
    currentDir <- getwd()
    filePath <- paste(currentDir,"/",directory,"/", sep='')
    
    env <- new.env(parent=emptyenv()) 
    env$fileVector <- c()
    env$completeCasesVector <- c()
    
    for (index in id)
    {
        fileName1 <- "00"
        fileName2 <- "0"
        fileName3 <- ""
        if(index < 10)
        {
            fileName1 <- paste(fileName1, index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName1, sep='')
            
            updateVectors(finalFileName, index, env)
        }
        else if(index >= 10 && index < 100)
        {
            fileName2 <- paste(fileName2,index, ".csv", sep='')
            finalFileName <- paste(filePath, fileName2, sep='')
            
            updateVectors(finalFileName, index, env)
        }
        else
        {
            fileName3 <- paste(index, ".csv" ,sep='')
            finalFileName <- paste(filePath, fileName3, sep='')
            
            updateVectors(finalFileName, index, env)
        }    
    }
    
    result <- data.frame(id = env$fileVector, nobs = env$completeCasesVector)
}