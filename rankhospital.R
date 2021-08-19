rankhospital <- function(state, outcome, num = "best"){
  care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!state %in% care_outcome$State){
    stop("invalid state")
  }else if (!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  data_frame <- care_outcome[ , c(2, 7, outcomes[outcome])]
  DfStateWise <- split(data_frame, data_frame[ ,2])
  DfContinentWise <- DfStateWise[[state]]
  DfContinentWise[ ,3] <- as.numeric(DfContinentWise[ ,3])
  OrderedDf <- DfContinentWise[order(DfContinentWise[ ,3], 
                                     DfContinentWise[ ,1]), ]
  OrderedDfWithoutNa <- OrderedDf[!is.na(OrderedDf[ ,3]), ]
  if (num == "best"){
  ##best <- which(as.numeric(OrderedDfWithoutNa[ ,3]) == 
                    ## min(as.numeric(OrderedDfWithoutNa[ ,3]), na.rm = TRUE))
   ## OrderedDfWithoutNa[num, 1]
   num <- 1
  }else if (num == "worst"){
    ## worst <- which(as.numeric(OrderedDfWithoutNa[ ,3]) == 
                   ## max(as.numeric(OrderedDfWithoutNa[ ,3]), na.rm = TRUE))
    ## OrderedDfWithoutNa[num, 1]
    num <- nrow(OrderedDfWithoutNa)
    ## num <- worst
  }
  OrderedDfWithoutNa[num, 1]
}


