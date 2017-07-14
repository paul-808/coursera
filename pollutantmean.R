# Part 1
# 
# Write a function named 'pollutantmean' that calculates the mean of a 
# pollutant (sulfate or nitrate) across a specified list of monitors. 
# The function 'pollutantmean' takes three arguments: 'directory', 
# 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean'
# reads that monitors' particulate matter data from the directory 
# specified in the 'directory' argument and returns the mean of the 
# pollutant across all of the monitors, ignoring any missing values coded 
# as NA. A prototype of the function is as follows
# 
# 
# You can see some example output from this function below. The function 
# that you write should be able to match this output. Please save your code 
# to a file named pollutantmean.R.
# 
# pollutantmean-demo.html

pollutantmean <- function(directory, pollutant, id){

  i <- 1L
  j <- length(id)
  id2 <- as.character(id)
  n <- 0
  total <- 0
  
  for (i in i:j){
    if (id[i] < 10) {
      id2[i] <- as.character(paste("00",id[i],sep=""))
    }
    if (id[i] >9 & id[i]<100){
      id2[i] <- as.character(paste("0",id[i], sep=""))
    }
  }
  
  paths <- paste(directory,"/",id2,".csv", sep="")
  
  for(i in i:j){
    data <- read.csv(paths[i],header = TRUE)
    total <- total + sum(na.omit(data[pollutant]))
    n <- n + nrow(na.omit(data[pollutant]))
  }
  
  total/n
}
