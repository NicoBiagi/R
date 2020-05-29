## THIS SCRIPT ANALYSISE THE EMG DATA FOR THE CORRUGATOR.
## BEFORE RUNNING THE SCRIPT CHECK THE SAMPLING RATE OF THE EMG DATA [AT THE MOMENT THE INTERVAL IS 0.001, SO 1000 SAMPLES MAKE A SECOND] 

# remove all the variables from the global environment
rm(list = ls())

# load the yarrr package and the tidyverse package
library("yarrr")
library("tidyr")
library("tidyverse")
library("lme4")
library("lmerTest")
library("foreign")
library("haven")
library("xlsx")
library("ez")
library("nlme")
library("multcomp")
library("readxl")
library("intervals")

# turn this off if you want scientific notation
options(scipen = 999)

# set the wd to the folder where the csv file is
# WINDOWS
if (Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/zj903545/OneDrive - University of Reading/Jayne/NEW PROJECT/")
} else{
  # MAC
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/NEW PROJECT/")
}

# get the list of all the .txt files in the folder
files_txt <- list.files(pattern = ".txt")

# this removes all the .txt files that are not relevant
y=1
files<-vector()
for (i in 1:length(files_txt)){
  if (str_detect(files_txt[i], "COR")==TRUE){
    files[y]<- files_txt[i]
    y<-y+1
  }
}

# create an empty variable for later
FINAL =NULL

#this creates a loop for all the partecipants
for (y in 1:length(files)){
  
  # print the filename
  print(files[y])
  
  # read the .txt file
  data <- read.delim(files[y], header=FALSE)
  
  # get all the times that the experiment was started
  start <- data[,2] == "StartOfBlock"
  
  # get the index for the start/starts of the experiment
  start_loc <- which(start == "TRUE")
  
  # create an empty variable for later
  Start = NULL
  
  # get the indeces to remove from the dataset
  for (t in 1:length(start_loc)){
    START <- start_loc[t]
    
    # get the index for the start of the exp and the 6 following indices
    S_rem <- START:(START+6)
    
    # convert it to a dataframe
    S_rem <-as.data.frame(S_rem)
    
    # save it outside the for-loop
    Start<- rbind(Start, S_rem)
  }
  
  # convert the indices from dataframe to numeric
  Start <- as.numeric(unlist(Start))
  
  # remove the indices from the dataset
  data<-data[-c(Start),]
  
  # the first column contains the time stamp info
  time <- data$V1
  
  # the second column contain the EMG scores
  values <- data$V2
  
  # convert the GSR values to numeric
  values<-as.numeric(as.character(values))
  
  # the third column contains the markers
  marker <-data$V3
  
  # convert the marker from factor to character
  marker1 <- as.character(marker)
  
  # get the unique characters in the marker variable
  all_marker <- unique(marker1)
  
  # this removes all the markers that are not relevant 
  x=1
  images <- vector()
  for (i  in 1:length(all_marker)){
    if (str_detect(all_marker[i], "image")==TRUE){
      images[x]<- all_marker[i]
      x <- x+1
    }
  }
  
  # sort the vector alphabetically
  images <- sort(images)
  
  # create an empty variable for later
  f = NULL
  
  for (i in 1:length(images)){
    # this takes the image and analyze it
    MARKER <- images[i]
    
    # this compares logically every marker with the image
    INDEX <- MARKER == marker1
    
    # this saves just the logic that are equal to TRUE
    LOCATIONS <- which(INDEX == "TRUE")
    
    # we want to analyze each second after the image-onset separately, and each needs 1000 samples
    n_cols <- 1000
    
    # create an empty variable for later
    d =NULL
    
    # get for all the images presentations, the baseline-corrected average for the 5 seconds following the images onset
    
    for (z in 1:length(LOCATIONS)){
      # get the specifc index for the n-image presentation
      LOC <- LOCATIONS[z]
      
      # get the average response for the second before the image onset [i.e., baseline]
      Baseline <- mean(values[seq(to = LOC, length.out = n_cols)],na.rm = TRUE)
      
      # get the average response for the 1st second after the image onset and then subtract the baseline
      After1 <- mean(values[seq(from = LOC, length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # get the average response for the 2nd second after the image onset and then subtract the baseline
      After2 <- mean(values[seq(from = (LOC+1000), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # get the average response for the 3rd second after the image onset and then subtract the baseline
      After3 <- mean(values[seq(from = (LOC+2000), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # get the average response for the 4st second after the image onset and then subtract the baseline
      After4 <- mean(values[seq(from = (LOC+3000), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # get the average response for the 5st second after the image onset and then subtract the baseline
      After5 <- mean(values[seq(from = (LOC+4000), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # combine the vectors into a dataframe
      trial <- cbind(After1, After2, After3, After4, After5)
      
      # convert it into a dataframe
      trial <- as.data.frame(trial)
      
      # rename the columns
      colnames(trial) <- c(paste(substring(MARKER,4,6),"T",z,"S1", sep=""), paste(substring(MARKER,4,6),"T",z,"S2", sep=""), paste(substring(MARKER,4,6),"T",z,"S3", sep=""), paste(substring(MARKER,4,6),"T",z,"S4", sep=""), paste(substring(MARKER,4,6),"T",z,"S5", sep=""))
      
      # save values outside the for-loop (1st iteration)
      if ((z==1)==TRUE){
        
        # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
        d<- rbind(d, trial)
        
        # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
        d_remove <- length(d)
      }
      
      # save the values outside the for-loop (2nd iteration and onwards)
      d<- cbind(d, trial)
      
      # empty the variable for the next iteration
      rm(trial)
    }
    
    # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
    d <- d[, -c(1:d_remove)]
    
    if ((i ==1)==TRUE){
      
      # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
      f <- rbind(f,d)
      
      # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
      col_remove <- length(d)
    }
    
    # save the values outside the for-loop
    f<- cbind(f,d)
    
    # empty the variable for the next iteration
    rm(d)
  }
  
  # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
  f <- f[, -c(1:col_remove)]
  
  # get the id 
  id <- as.factor(substring(files[y],1,6))
  
  # add the id name to the dataset
  f <- cbind(id,f)
  
  # save the values outside the for-loop
  FINAL <-rbind(FINAL, f)
}

# save the dataset as a csv file
write.csv(FINAL, file = "COR_FINAL.csv")
# write.csv(FINAL, file = paste(substr(files[y],1,nchar(files[y])-4),".csv", sep =""))
