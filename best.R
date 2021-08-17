best <- function(state, outcome){
  care_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## a vector containing the column Number of the required outcome
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!state %in% care_outcome$State){
    stop("invalid state")
  }else if (!outcome %in% names(outcomes)){
    stop("invalid outcome")
  }
 data_frame <- care_outcome[ , c(2, 7, outcomes[outcome])]
 data_frame_st <- split(data_frame, data_frame$State)
 df_state <- data_frame_st[[state]]
 min_rate <- which(as.numeric(df_state[,3]) == min(as.numeric(df_state[,3]), na.rm = TRUE))
 hospitals <- df_state[min_rate, 1]
 hospital <- sort(hospitals)
 hospital[1]
}
