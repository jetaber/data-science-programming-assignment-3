rankhospital <- function(state, outcome, num = "best") {
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
   
   outcomeCol <- 0
   if (outcome == "heart attack") {
      outcomeCol <- 11
      oocm[, 11] <- as.numeric(oocm[, 11])
   } else if (outcome == "heart failure") {
      outcomeCol <- 17
      oocm[, 17] <- as.numeric(oocm[, 17])
   } else if (outcome == "pneumonia") {
      outcomeCol <- 23
      oocm[, 23] <- as.numeric(oocm[, 23])
   }
  
   
   #subset on state
   allOutcomesInState <- subset(oocm,(oocm[7] == state))
   #print(allOutcomesInState[1:10,2])
   
   rankedOutcomesInState <- allOutcomesInState[order(allOutcomesInState[,outcomeCol],allOutcomesInState[,2],na.last=NA),]
  #print(order(allOutcomesInState[,outcomeCol]))
  
    rank <- 0
    if (num == "best") rank <- 1
    else if (num == "worst") rank <- nrow(rankedOutcomesInState)
    else rank <- as.numeric(num)
  
   rankedOutcomesInState[rank,2]
  
}
