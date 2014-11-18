#################################
# complete
#
#################################
setwd("c:/specdata")
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  mainDir <- "C:/"
  directoryName <- paste(mainDir, directory, sep = "")
  
  #Create a empty data.frame to hold id's and nobs(number of observations)
  data <- data.frame()
  
  #Create a empty vector to hold monitor id's
  idsvect <- vector("integer")
  
  #Create a empty vector to hold nobs(number of observations)
  nobsvect <- vector("integer")
  
  #Create a empty list to hold all data
  mylist <- list()
  
  #List files in the directory
  filelisting <- list.files(directoryName)
  
  #Get the vector length
  idval <- length(filelisting[id]) 
  
  mondata <- numeric(idval)
 
  
  #Process multiple id monitor data files
 
    for (i in id)
    {
      #monitor is a data.frame of monitor data similar to list
      monitor <- read.csv(paste(directoryName, "/", filelisting[i], sep = ""))
      ids <- monitor$ID[1]
      idName <- names(monitor[4]) 
      
      #Bind sulfate and nitrate columns and will give two columns one is sulfate and other nitrate
      #and returns a list
      nit_sulfList <- cbind(monitor$sulfate,monitor$nitrate)
      
      #Convert the binded data of nitrate and sulfate to a data.frame
      nit_sulfDF <- as.data.frame(nit_sulfList)
      
      #Get the complete cases for both columns (i.e. both nitrate and sulfate row must be not NA for each row)
      goodMonitors <- complete.cases(nit_sulfDF)
      
      #Get the count of complete nobs
      cnt <- nrow(nit_sulfDF[goodMonitors, ][, ]) #gets the complete cases for nitrate and sulfate on same row where
      
      idsvect <- c(idsvect,ids)
      nobsvect <- c(nobsvect, cnt)  
      
    }
   values <- as.data.frame(list(id = idsvect, nobs = nobsvect))
   
   #print(idsvect)
   #print(nobsvect)
   return(values)
  #return(mymean)
  
}
