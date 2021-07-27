## week 2 assignment 1 part 1
pollutantmean <- function(directory, pollutant, id){
  my_files <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
  x <- numeric()
  for (i in id) {
    req_data <- read.csv(my_files[i])
    x <- c(x, req_data[[pollutant]])
  }
  mean(x, na.rm = TRUE)
}

## week 2 assignment 1 part 2
complete <- function(directory, id = 1:332){
  my_files <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()
  for (i in id) {
    req_data <- read.csv(my_files[i])
    y <- complete.cases(req_data)
    mysum <- sum(y)
    nobs <- c(nobs, mysum)
  }
  data.frame(id, nobs)
}

## week 2 assignment 1 part 3

corr <- function(directory, threshold = 0){
  my_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  observatons <- complete(directory)
  actual_data <- observatons[["nobs"]]
  id <- 1:332
  crr <- numeric()
  for (i in id) {
    if (actual_data[i] > threshold){
      read_my_files <- read.csv(my_list[i])
      completecases_read_my_files <- complete.cases(read_my_files)
      read_my_files_withoutNA <- read_my_files[completecases_read_my_files, ]
      crr <- c(crr, cor(read_my_files_withoutNA[["sulfate"]], read_my_files_withoutNA[["nitrate"]]))
    }
  }
  crr
}
