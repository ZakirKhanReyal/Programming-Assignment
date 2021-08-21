rankall <- function(outcome, num = "best"){
  care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
  data_frame <- care_outcome[ , c(2, 7, outcomes[outcome])]
  data_frame[ ,3] <- as.numeric(data_frame[ ,3])
  DfWithoutNa <- data_frame[!is.na(data_frame[ ,3]), ]
  if (num == "best"){
    DfStateWise <- split(DfWithoutNa, DfWithoutNa[ ,2])
    DfOfInterest <- data.frame(Hospital = character(), 
                               state = character(), stringsAsFactors = FALSE)
    for (i in seq_along(DfStateWise)) {
      DfContinentWise <- DfStateWise[i][[1]]
      OrderedDf <- DfContinentWise[order(DfContinentWise[ ,3],
                                         DfContinentWise[ ,1]), ]
      num <- 1
      HospitalOnContinental <- OrderedDf[num, c(1, 2)]
      ContinentName <- DfContinentWise[2][[1]][1]
      HospitalOnContinental[ ,2] <- ContinentName
      DfOfInterest[i, ] <- HospitalOnContinental
    }
  }else if (num == "worst"){
    DfStateWise <- split(DfWithoutNa, DfWithoutNa[ ,2])
    DfOfInterest <- data.frame(Hospital = character(), 
                               state = character(), stringsAsFactors = FALSE)
    for (i in seq_along(DfStateWise)) {
      DfContinentWise <- DfStateWise[i][[1]]
      OrderedDf <- DfContinentWise[order(DfContinentWise[ ,3],
                                         DfContinentWise[ ,1]), ]
      num <- nrow(OrderedDf)
      HospitalOnContinental <- OrderedDf[num, c(1, 2)]
      ContinentName <- DfContinentWise[2][[1]][1]
      HospitalOnContinental[ ,2] <- ContinentName
      DfOfInterest[i, ] <- HospitalOnContinental
    }
  }else{
    DfStateWise <- split(DfWithoutNa, DfWithoutNa[ ,2])
    DfOfInterest <- data.frame(Hospital = character(), 
                               state = character(), stringsAsFactors = FALSE)
    for (i in seq_along(DfStateWise)) {
      DfContinentWise <- DfStateWise[i][[1]]
      OrderedDf <- DfContinentWise[order(DfContinentWise[ ,3],
                                         DfContinentWise[ ,1]), ]
      HospitalOnContinental <- OrderedDf[num, c(1, 2)]
      ContinentName <- DfContinentWise[2][[1]][1]
      HospitalOnContinental[ ,2] <- ContinentName
      DfOfInterest[i, ] <- HospitalOnContinental
    }
  }
  DfOfInterest
}

  
  
  
  
  
  
  
  
  