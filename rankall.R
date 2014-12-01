#################################
# rankall
# Rank the hospitals in all states by best, worst and a a ranking number
#
#################################
setwd("c:/specdata")

conditions <- list("heart attack", "heart failure", "pneumonia")

rankhospitals <- vector("character")
rankstates <- vector("character")

###############################################################
# rankall - return ranked hospitals and states                #
#                                                             #
#                                                             #
###############################################################
rankall <- function(outcome, num = "best") {
        #Validate outome before proceeding
        vald <- FALSE
        verifyOutome(vald, conditions, outcome)
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
       
        mainDir <- getwd()
        file <- "outcome-of-care-measures.csv"
        
        stateStorage <- vector()
        hospStorage <- list()
        heartAttackDf <- data.frame(stringsAsFactors = FALSE)
        
        #hosp.data is dataframe of entire hospital data file
        hosp.data <- read.csv(paste(mainDir, "/", file , sep = ""), na.strings = "Not Available", stringsAsFactors=F)
        hosp.rows <- nrow(hosp.data)
        hosp.col  <- ncol(hosp.data)
        
        #Get a listing of all the states from the hospital ratings dataframe and remove duplicate state names
        stateStorage <- sort(unique(hosp.data$State))
               
        
        #####################################################
        # Get best mortality for HEART ATTACK               #
        #####################################################
        
        if(outcome == "heart attack"){      
                outcome.column.name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                ranks <- processOutcome(outcome, hosp.data, hosp.rows, c(2,7,11), stateStorage, outcome.column.name, num)          
                        
        }
        
        #####################################################
        # Get best mortality for HEART FAILURE              #
        #####################################################
        
        if(outcome == "heart failure"){      
                outcome.column.name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                ranks <- processOutcome(outcome, hosp.data, hosp.rows, c(2,7,17), stateStorage, outcome.column.name, num)          
                
        }
        
        #####################################################
        # Get best mortality for PNEUMONIA                  #
        #####################################################
        
        if(outcome == "pneumonia"){      
                outcome.column.name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                ranks <- processOutcome(outcome, hosp.data, hosp.rows, c(2,7,23), stateStorage, outcome.column.name, num)          
                
        }
        
        return(ranks)
}

#####################################################################
#processOutcome
#Return the rankings of the hospitals and number
#####################################################################
processOutcome <- function(outcome, hospdata, hosprows, colvect, numstates, columnname, num, ... ){
        
        
                #Get Hospital Name, State and Pneumonia column data from data frame
                outcomeDf <- hospdata[1:hosprows,colvect] 
                
                for (i in 1:length(numstates)){        
                        
                        #Subset the dataframe and get all vertical rows from the data frame for a particular index that match the state 
                        outcomeByState <- outcomeDf[which(outcomeDf[,"State"] == numstates[i]),]
                        
                        #Subset the heart attack dataframe and ignore rows with NA's from Heart Attack column(i.e. col 3 all rows)
                        outcomeNoNaDf <- subset(outcomeByState,  outcomeByState[,3] != "NA") 
                        
                        if(outcome == "pneumonia")
                           #Order the data
                           orderit <- outcomeNoNaDf[order(outcomeNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]  
                        
                        if(outcome == "heart failure")
                           #Order the data
                           orderit <- outcomeNoNaDf[order(outcomeNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                        
                        if(outcome == "heart attack"){
                           #Order the data by the Heart Attack column
                           orderit <- outcomeNoNaDf[order(outcomeNoNaDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                        }
                        
                        #Process the data and get the value with the rating based on the num value
                        orderit <- processRates(orderit, outcomeNoNaDf, num, outcome)
                        
                        #Store ranked hospitals
                        rankhospitals <- c(rankhospitals,orderit[1])
                        
                        #Store corresponding ranked states
                        rankstates <- c(rankstates,numstates[i])
                }
                
                #Create the data frame with ranked hospitals and the corresponding state to screen
                #replaced the values in row.names with the rankstates ************REMEMBER THIS ***************
                ranks <- data.frame(row.names=rankstates, hospital=rankhospitals, state=rankstates) 
        
}

####################################################################
# processRates - depending on num determine the rankings
# dfObj1 = orderit object
# dfObj2 = outcome dataframe object
# num = best,worst, or a number 
# conditionType = outcome type heart attack,pneumonia,heart failure
# Returns: the matched hospital at the num position
####################################################################

processRates <- function(dfObj1,dfObj2, num, conditionType){
        
        hfrows <- nrow(dfObj1)
        
        if(num == "best"){
                #Get the hospital with the lowest(best) rating which is in first position of dataframe
                dfObj1 <- dfObj1[1,1]        
                
        }else if(num == "worst"){
                #Get the worst rating which is in the last entry of the dataframe
                dfObj1 <- dfObj1[hfrows,1]        
                
        }else if(is.numeric(num)){
                #Get a ranking by passing in a numeric value and check against the data
                
                #Check if the num passed in for ranking is greater than the number of hospitals in our list
                #if so than print NA
                if(num > hfrows){
                        dfObj1 <- "NA"     
                }else{
                        #Check the ratings column to see if any identical ratings     
                        duprates <- head(dfObj1[,3],num)
                        
                        #Get duplicate vector length
                        mylen <- length(duprates[duplicated(duprates)])
                        
                        if(mylen >= 1){
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
                        }else{
                                #If no duplcates than process as normal and return hospital name
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

verifyOutome <- function(vald, cond, outc){
        #Verify outome value and If found return a list TRUE or FALSE for the outcome that matched
        outcome.found <- lapply(cond,function(elt) elt[1:length(elt)] == outc)
        
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
        if(!vald) stop("invalid outcome")
        
}
