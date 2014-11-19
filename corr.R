#################################
# corr
#
#################################
setwd("c:/specdata")
source("complete.R")
corr <- function(directory, threshold = 0) {
  
  mainDir <- "C:/"
  directoryName <- paste(mainDir, directory, sep = "")
  results <- vector()
  complete.cases = complete("specdata")
  
  #For all the complete cases grab the rows from the data frame
  #that is returned that are greater than the threshold
  datatobe.correlated = subset(complete.cases, nobs > threshold)
  
  #Get number of rows for data frame datatobe.correlated
  complete.rows <- nrow(datatobe.correlated)
  #print(complete.rows)
  
  #loop through the complete cases dataframe and correlate the sulfate and nitrate
  #columns for each monitor
  for (i in c(1:complete.rows)){
    #print(i)
    #filename <- getfilename(datatobe.correlated$id)
    filename <- getfilename(datatobe.correlated$id[i])
    monitor <- read.csv(paste(directoryName, "/", filename, sep = ""))
    
    #Bind sulfate and nitrate columns and will give two columns one is sulfate and other nitrate
    #and returns a list
    nit_sulfList <- cbind(monitor$sulfate,monitor$nitrate)
    
    #Convert the binded data of nitrate and sulfate to a data.frame
    nit_sulfDF <- as.data.frame(nit_sulfList)
    
    #Get the complete cases for both columns (i.e. both nitrate and sulfate row must be not NA for each row)
    goodMonitors <- complete.cases(nit_sulfDF)
    
    #Get the count of complete nobs
    zx <- nit_sulfDF[goodMonitors, ][, ] #gets the complete cases for nitrate and sulfate on same row where
    cordata <- cor(c(zx$V1),c(zx$V2))
    results <- c(results,cordata)
    #print(head(zx))        
  }
  print(results)
  
}

getfilename <- function(id){
  
  if (nchar(id)==1) {
    
    sid <- paste("00",id,sep='')
  }
  else if (nchar(id)==2) {
    sid <- paste("0",id,sep='')
  }
  else {
    sid <- id
  }
  sid <- paste(sid,".csv", sep="")
  return(sid)
}
