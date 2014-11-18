#################################
# pollutantmean
#
#################################
setwd("c:/specdata")
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  mainDir <- "C:/"
  directoryName <- paste(mainDir, directory, sep = "")
  
  
  #List files in the directory
  filelisting <- list.files(directoryName)
  idval <- length(filelisting[id]) #Get the vector length
  mondata <- numeric(idval)
  
  #Process multiple id monitor data files
  if(idval > 1){
    for (i in id)
    {
      #monitor is a data.frame of tabular data similar to list
      monitor <- read.csv(paste(directoryName, "/", filelisting[i], sep = ""))
      #print(monitor$nitrate)
      mon <- c(monitor[, pollutant])
      #Build a numeric vector of monitor data for all monitors
      mondata <- c(mondata, mon)
    }
      
  }
  mymean <- mean(mondata, na.rm = TRUE)
  
  #print(monitor)
  
  #Process single id monitor data files
  if(idval == 1){
     #Process 1 monitor id of data
     monitor <- read.csv(paste(directoryName, "/", filelisting[id], sep = ""))
     mymean <- round(calcMean(pollutant,monitor),3)
     
  }
  return(mymean)

}

#################################
# calcMean function
#
#################################
calcMean <- function(p,mon){
  mypollutant <- mon[, p] #Grab the column with the correct pollutant and run mean
  mymean <- mean(mypollutant, na.rm = TRUE)
  
}

getPollutant <- function(p,mon){
  mypollutant <- mon[, p] #Grab the column from the data.frame monitor object with the correct pollutant
}

allMonitorMeans <- function(p,mon){
  mypollutant <- mon[, p] #Grab the column with the correct pollutant and run mean
  mymean <- mean(mypollutant, na.rm = TRUE)
  
}

finalMean <- function(m){
  mymean <- mean(m, na.rm = TRUE)
  
}
