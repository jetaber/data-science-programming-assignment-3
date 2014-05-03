

best <- function(state, outcome) {
   ## Read outcome data
   oocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
   #check valid outcome
   
   if (! any(c("heart attack", "heart failure", "pneumonia") == outcome)) {
      stop ("invalid outcome")
   } 
   
   #check valid state
   if (! any(state.abb == state)) {
      stop ("invalid state")
   }
   
   if (outcome == "heart attack") {
      oocm[, 11] <- as.numeric(oocm[, 11])

      #subset on state
      allOutcomesInState <- subset(oocm,(oocm[7] == state))
      
      #get lowest
      lowestHeartAttackInState <- subset(allOutcomesInState,allOutcomesInState[11] == min(allOutcomesInState[,11],na.rm=TRUE))
            
      #sort lowest alphabetically 
      return(sort(lowestHeartAttackInState[,2]))
   }
   
   if (outcome == "heart failure") {
      oocm[, 17] <- as.numeric(oocm[, 17])
      
      #subset on state
      allOutcomesInState <- subset(oocm,(oocm[7] == state))
      
      #get lowest
      lowestHeartFailureInState <- subset(allOutcomesInState,allOutcomesInState[17] == min(allOutcomesInState[,17],na.rm=TRUE))
      
      #sort lowest alphabetically 
      print(sort(lowestHeartFailureInState[,2]))
   }
   
   if (outcome == "pneumonia") {
      oocm[, 23] <- as.numeric(oocm[, 23])
      
      #subset on state
      allOutcomesInState <- subset(oocm,(oocm[7] == state))
      
      #get lowest
      lowestPneumoniaInState <- subset(allOutcomesInState,allOutcomesInState[23] == min(allOutcomesInState[,23],na.rm=TRUE))
      
      #sort lowest alphabetically 
      print(sort(lowestPneumoniaInState[,2]))
   }
   
   
   
  
   ## Return hospital name in that state with lowest 30-day death
   ## rate
   
   
   
   
}
