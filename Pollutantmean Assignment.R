#Programming Assignment: Air Pollution
#Author: Tianxiao Fan
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

#Part 2
#Write a function that reads a directory full of files
#and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file
#and the second column is the number of complete cases

directory<-setwd('C:/Users/Tianx/OneDrive/Documents/R/02-R Programming/specdata/')

complete <-function(directory, id=1:332){
  directory <- setwd('C:/Users/Tianx/OneDrive/Documents/R/02-R Programming/specdata/')
  filelist <-list.files(path=directory, pattern=".csv", full.names = TRUE)
  nobs <- numeric()
  
  for (i in id){
    nobs <- c(nobs,sum(complete.cases(read.csv(filelist[i])))) 
  }  
  
  data.frame(id, nobs)
}

complete(directory,1:3)

#Part 3
#Write a function that takes a directory of data files and a threshold
#for complete cases and calculates the correlation between sulfate
#and nitrate for monitor locations where the number of completely observed cases
#(on all variables) is greater than threshold.
#The function should return a vector of correlations for the monitors 
#that meet the threshold requirement. If no monitors meet the threshold requirement
#then the function should return a numeric vector of length 0.

corr <- function(directory, threshold=0){
  filenames <-list.files(directory)
  filenames <- paste(directory,"//",filenames,sep="")
  all_files<- complete(directory)
  cor_vector <- numeric()
  for (i in 1:nrow(all_files)){
    if (all_files[i,2]>threshold){
      cor_vector <- c(cor_vector,cor(read.csv(filenames[i])$sulfate,read.csv(filenames[i])$nitrate, use = "pairwise.complete.obs"))
    }
  }  
  cor_vector
}
