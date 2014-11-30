#################################
# best
#
#################################
setwd("c:/specdata")

#outcome conditions
conditions <- list("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        vald <- FALSE
        mainDir <- getwd()
        file <- "outcome-of-care-measures.csv"
        heartAttackDf <- data.frame(stringsAsFactors = FALSE)
        
        #hosp.data is dataframe of entire hospital data file
        hosp.data <- read.csv(paste(mainDir, "/", file , sep = ""), na.strings = "Not Available", stringsAsFactors=F)
        hosp.rows <- nrow(hosp.data)
        hosp.col  <- ncol(hosp.data)
        
        #Validate State argument
        #Pass in dataframe object, state to determine if state is valid
        stValid <- checkState(hosp.data,state)
        
        #Look for the outcome value in the conditions list. 
        #If found return a list TRUE or FALSE for the outcome that matched
        outcome.found <- lapply(conditions,function(elt) elt[1:length(elt)] == outcome)
        
        #Convert to logical vector in order to test for logical values since lapply returns a list
        outcome.found <- as.logical(outcome.found)
        
        #Check for TRUE value in the logical list and if any value is TRUE than we found our outcome
        for (i in 1:3){
                if(outcome.found[i] == TRUE){
                   vald <- TRUE;
                }else{
                   next;        
                }  
        }
      
        #Validate outcome argument
        if(!vald) stop("invalid outcome")
            
        
        ###############################################
        ##Get best mortality for Heart Attack Outcome
        ###############################################k
        if(outcome == "heart attack"){          
          
                #Get Hospital Name(column 2), State(column 7) and Heart Failure(column 11) column data from data frame
          heartAttackDf <- hosp.data[1:hosp.rows,c(2,7,11)] 
          
          #Subset the dataframe and get all vertical rows from the data frame for a particular index that match the stat 
          heartAttackByState <- heartAttackDf[which(heartAttackDf[,"State"] == state),]
          
          #Subset the heart attack dataframe object for all rows and remove NA's from Heart Failure column(i.e. col 3)
          heartAttackNoNaDf <- subset(heartAttackByState,  heartAttackByState[,3] != "NA") 
          
          #Order the heart attack data by ratings
          orderit <- heartAttackNoNaDf[order(heartAttackNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
       
        }
        
        ###############################################
        ##Get best mortality for Heart Faliure Outcome
        ###############################################
        if(outcome == "heart failure"){
                #Get Hospital Name(column 2), State(column 7) and Heart Failure(column 17) column data from data frame
                heartFailDf <- hosp.data[1:hosp.rows,c(2,7,17)] 
                
                #Subset the dataframe and get all vertical rows from the data frame for a particular index that match the state 
                heartFailByState <- heartFailDf[which(heartFailDf[,"State"] == state),]
                
                #Subset the heart failure dataframe object for all rows and remove NA's from Heart Failure column(i.e. col 3)
                heartFailNoNaDf <- subset(heartFailByState,  heartFailByState[,3] != "NA") 
                
                #Order the heart failure data by ratings
                orderit <- heartFailNoNaDf[order(heartFailNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]               
        }
        
        ###############################################
        ##Get best mortality for Pneumonia Outcome
        ###############################################
        if(outcome == "pneumonia"){
                #Get Hospital Name(column 2), State(column 7) and Pneumonia(column 23) column data from data frame
                pneumoniaDf <- hosp.data[1:hosp.rows,c(2,7,23)] 
                
                #Subset the dataframe and get all vertical rows from the data frame for a particular index that match the state 
                pneumoniaByState <- pneumoniaDf[which(pneumoniaDf[,"State"] == state),]
                
                #Subset the pneumonia dataframe object for all rows and remove NA's from Pneumonia column(i.e. col 3)
                pneumoniaNoNaDf <- subset(pneumoniaByState,  pneumoniaByState[,3] != "NA") 
                
                #Order the pnemonia data by ratings
                orderit <- pneumoniaNoNaDf[order(pneumoniaNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]       
        
        }
        
        #Return the first element and column.     
        return(orderit[1,1])
        
        #return(head(orderit,4)) for testing output
}



####################################################
# checkState
# Description: check that the state passed in is in the dataset
# Arguments: dataframe object, state
#
####################################################
checkState <- function(data, state, isValid = TRUE){
              
              #Get State column data or could have split it using st <- split(hosp.data$State,hosp.data$State) and 
              #get the names(st)
              allState1 <- data$State 
              
              #Subset the state vectore and search it to see if state is valid i.e. found
              st <- allState1[allState1 == state]

              #If st vector is empty than the state is invalid and stop the process, else return TRUE result
              if(length(st) == 0){
                      stop("invalid state")
              }else{
                isValid      
              }    
}
