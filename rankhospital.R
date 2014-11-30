#################################
# rankhospital
# Rank the hospitals depending on the best value: best, worst and a number
#
#################################
setwd("c:/specdata")

conditions <- list("heart attack", "heart failure", "pneumonia")

rankhospital <- function(state, outcome, num = best) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        vald <- FALSE
        mainDir <- getwd()
        file <- "outcome-of-care-measures.csv"
        vectStorage <- vector()
        listStorage <- list()
        matrixStorage <- matrix()
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
        
        
        
        #Validate outcome argument
        if(vald == TRUE) {
                # print(outcome)
        }else{
                stop("invalid outcome")
        }
        
        #####################################################
        # Get best mortality for HEART ATTACK               #
        #####################################################
        if(outcome == "heart attack"){          
                
                #Get Hospital Name, State and Heart Attack data from data frame
                heartAttackDf <- hosp.data[1:hosp.rows,c(2,7,11)] 
                
                #Get all row index that match the state 
                heartAttackByState <- heartAttackDf[which(heartAttackDf[,"State"] == state),]
                
                #Subset the heart attack dataframe and remove NA's from Heart Attack column(i.e. col 11 all rows)
                heartAttackNoNaDf <- subset(heartAttackByState,  heartAttackByState[,3] != "NA") 
                #Convert the column to numeric as it was a character
                #heartAttackNoNaDf <- transform(heartAttackNoNaDf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                
                orderit <- heartAttackNoNaDf[order(heartAttackNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                orderit <- processRates(orderit, heartAttackNoNaDf, num, outcome)
               
        }
        
        
        #####################################################
        # Get best mortality for HEART FAILURE              #
        #####################################################
        if(outcome == "heart failure"){
                #Get Hospital Name, State and Heart Failure data from data frame
                heartFailDf <- hosp.data[1:hosp.rows,c(2,7,17)] 
                
                #Get all row index that match the state 
                heartFailByState <- heartFailDf[which(heartFailDf[,"State"] == state),]
                
                #Subset the heart attack dataframe and remove NA's from Heart Attack column(i.e. col 11 all rows)
                heartFailNoNaDf <- subset(heartFailByState,  heartFailByState[,3] != "NA") 
                #heartFailNoNaDf <- transform(heartFailNoNaDf, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                orderit <- heartFailNoNaDf[order(heartFailNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
                
                orderit <- processRates(orderit, heartFailNoNaDf, num, outcome)
                
        }
        
        
        #####################################################
        # Get best mortality for PNEUMONIA                  #
        #####################################################
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
                
                orderit <- processRates(orderit, pneumoniaNoNaDf, num, outcome)
                
        }
        
        
        return(orderit)
}

###########3
# dfObj1 = orderit object
# dfObj2 = outcome dataframe object
# num = best,worst, or a number 
# conditionType = outcome type heart attack,pneumonia,heart failure
#

processRates <- function(dfObj1,dfObj2, num, conditionType){
        
        hfrows <- nrow(dfObj1)
        
        if(num == "best"){
                #Get the hospital with the lowest rating which is in first position of dataframe
                dfObj1 <- dfObj1[1,1]        
                
        }else if(num == "worst"){
                #Get the worst rating which is in the last entry of the dataframe
                dfObj1 <- dfObj1[hfrows,1]        
                
        }else if(is.numeric(num)){
                #Check if the num passed in for ranking is greater than the number of hospitals in our list
                #if so than print NA
                if(num > hfrows){
                     dfObj1 <- "NA"     
                }else{
                        #Check the ratings column to see if any identical ratings     
                        duprates <- head(dfObj1[,3],num)
                        if(length(duprates[duplicated(duprates)]) >= 1){
                                #We have identical ratings for the listing so break the tie and order by Hospital.Name
                                if(conditionType == "heart failure"){
                                  dfObj1 <- dfObj2[order(dfObj2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,dfObj2$Hospital.Name),]
                                }
                                
                                if(conditionType == "heart attack"){
                                  dfObj1 <- dfObj2[order(dfObj2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,dfObj2$Hospital.Name),]
                                }
                                
                                if(conditionType == "pneumonia"){
                                        dfObj1 <- dfObj2[order(dfObj2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,dfObj2$Hospital.Name),]
                                }
                                
                                dfObj1 <- head(dfObj1,num) #Get the number of ratings
                                dfObj1 <- dfObj1[num,1] #Retrieve the Hospital Name
                        }
                }  
        } else {
                dfObj1 <- "NANA"        
        }
        
        return(dfObj1)
        
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
