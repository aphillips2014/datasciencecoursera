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
        
        #Convert to logical vector in order to test for logical values
        outcome.found <- as.logical(outcome.found)
        
        #Check for TRUE value in the logical list and if any value is TRUE than we found our outcome
        for (i in 1:3){
                if(outcome.found[i] == TRUE){
                   vald <- TRUE;
                }else{
                   next;        
                }  
        }
      
        #if(outcome.found[1:length(conditions) == TRUE]) {
        
        #Validate outcome argument
        if(vald == TRUE) {
              # print(outcome)
        }else{
                stop("invalid outcome")
        }
        
        #Get best mortality for heart attack
        if(outcome == "heart attack"){          
          
          #Get Hospital Name, State and Heart Attack data from data frame
          heartAttackDf <- hosp.data[1:hosp.rows,c(2,7,11)] 
          
          #Get all row index that match the state 
          heartAttackByState <- heartAttackDf[which(heartAttackDf[,"State"] == state),]
          
          #Subset the heart attack dataframe and remove NA's from Heart Attack column(i.e. col 11 all rows)
          heartAttackNoNaDf <- subset(heartAttackByState,  heartAttackByState[,3] != "NA") 
          
          #Convert the column to numeric as it was a character
          #heartAttackNoNaDf <- transform(heartAttackNoNaDf, 
                                         #Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
                                         #= as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
          
          orderit <- heartAttackNoNaDf[order(heartAttackNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
       
        }
        
        if(outcome == "heart failure"){
                #Get Hospital Name, State and Heart Failure data from data frame
                heartFailDf <- hosp.data[1:hosp.rows,c(2,7,17)] 
                
                #Get all row index that match the state 
                heartFailByState <- heartFailDf[which(heartFailDf[,"State"] == state),]
                
                #Subset the heart attack dataframe and remove NA's from Heart Attack column(i.e. col 11 all rows)
                heartFailNoNaDf <- subset(heartFailByState,  heartFailByState[,3] != "NA") 
                #heartFailNoNaDf <- transform(heartFailNoNaDf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                
                orderit <- heartFailNoNaDf[order(heartFailNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]               
        }
        
        if(outcome == "pneumonia"){
                #Get Hospital Name, State and Pneumonia column data from data frame
                pneumoniaDf <- hosp.data[1:hosp.rows,c(2,7,23)] 
                
                #test <- which(pneumoniaDf[,"State"] == state)
                #Subset the dataframe and get all vertical rows from the data frame for a particular index that match the state 
                pneumoniaByState <- pneumoniaDf[which(pneumoniaDf[,"State"] == state),]
                
                #Subset the heart attack dataframe and remove NA's from Heart Attack column(i.e. col 11 all rows)
                pneumoniaNoNaDf <- subset(pneumoniaByState,  pneumoniaByState[,3] != "NA") 
                #pneumoniaNoNaDf <- transform(pneumoniaNoNaDf, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                
                orderit <- pneumoniaNoNaDf[order(pneumoniaNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]       
        
        }
        
        #Return the first element and column. Need to add code for a tie.
        
        return(orderit[1,1])
        #return(head(orderit,4)) for testing output
}




checkState <- function(data, state, isValid = TRUE){
              
              #Get State column data or could have split it using st <- split(hosp.data$State,hosp.data$State) and 
              #get the names(st)
              allState1 <- data$State 
              st <- allState1[allState1 == state]

              if(length(st) == 0){
                      stop("invalid state")
              }else{
                isValid      
              }    
}
