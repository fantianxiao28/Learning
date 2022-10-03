#Programming Assignment: Air Pollution
#Tianxiao Fan
#02 October 2022

#Part 1
#Write a function named 'pollutantmean' that 
#calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 
#'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' 
#reads that monitors' particulate matter data 
#from the directory specified in the 'directory' argument
#and return the mean of the pollutant across all of the monitors,
#ignoring any missing values coded as NA.


pollutantmean <- function(directory, pollutant, id=1:332){
  directory <- setwd('C:/Users/Tianx/OneDrive/Documents/R/02-R Programming/specdata/')
  filelist <- list.files(directory)
  data <- data.frame()
  
  for (i in id){
  data <- rbind(data,read.csv(paste(directory,"/",filelist[i],sep="")))
  }
  
  mean(data[,pollutant],na.rm=TRUE)
}

pollutantmean("specdata", "nitrate", 23)