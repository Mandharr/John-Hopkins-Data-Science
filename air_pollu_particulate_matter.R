# Dataset of 332 csv files contains pollution monitoring data for fine pariculte matter(PM) air pollution at 332 location in USA.
# Each file contains data from single monitor, with the ID number. 
# The files are unzipped and have missing values.

# Part1
# Write a function "pollutantmean" that calculates the mean of pollutants, when certain arguments are supplied.

pollutantmean <-  function(directory, pollutant, id = 1:332) {
  direct <- list.files(directory, full.names = TRUE) [id]
  dataset = data.frame()
  id <- as.integer(length(direct))
  for(i in 1:id){
    dataset <- rbind(dataset, read.csv(direct[i]))
  }
  
  mean(dataset[,pollutant], na.rm = TRUE)
}

