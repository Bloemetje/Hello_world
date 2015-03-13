best <- function(state, outcome){
  ## read data, data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## simplyfied data
    data2 <- data[,c(2,7,11,17,23)]
    
  ## subset data on statename
    logic.state <- vector()
    data.state <- data2[,2]
    logic.state <- state == data.state
    if (sum(logic.state) == 0) {
      stop("invalid state")
    }
    data.state.subset <- data2[logic.state,]

  ## subset data on outcome   
      if (outcome == "heart attack") {
        x <- 3
      }
      if (outcome == "heart failure") { 
        x <- 4
      }
      if (outcome == "pneumonia") {
        x <- 5
      }
  
      if (sum(outcome == c("heart attack","heart failure","pneumonia")) == 0 ){
        stop("invalid outcome")
      }
  
  data.state.subset[,x] <- as.numeric(data.state.subset[,x])
  NArm <- is.na(data.state.subset[,x])
  data.state.subset.NArm <- data.state.subset[!NArm,]

  z <-min(data.state.subset.NArm[,x])
  y <- data.state.subset.NArm[,x] == z
    
    bestH <- data.state.subset.NArm[y,]
    best <- (bestH[1,1])
    as.character(best)
    best
  }
