## THIS SCRIPT ANALYSISE THE EMG DATA FOR THE CORRUGATOR FOR THE REW TASK
## BEFORE RUNNING THE SCRIPT CHECK THE SAMPLING RATE OF THE EMG DATA [AT THE MOMENT THE INTERVAL IS 0.05, SO 20 SAMPLES MAKE A SECOND] 

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
  setwd('C://Users/zj903545/OneDrive - University of Reading/Jayne/REW/Exported/')
} else{
  # MAC
  setwd('/Users/nico/OneDrive - University of Reading/Jayne/REW/Exported/')
}

# get the list of all the .txt files in the folder
files_txt <- list.files(pattern = ".txt")

# this removes all the .txt files that are not relevant
y=1
files<-vector()
for (i in 1:length(files_txt)){
  if (str_detect(files_txt[i], "REW")==TRUE){
    files[y]<- files_txt[i]
    y<-y+1
  }
}

# create two empty variables for later
FINAL =NULL
FINAL2 = NULL

#this creates a loop for all the partecipants
for (y in 1:length(files)){
  
  # print the filename
  print(files[y])
  
  # read the .txt file
  data2 <- read.delim(files[y], header=FALSE)
  
  # get all the times that the experiment was started
  start <- data2[,2] == "StartOfBlock"
  
  # get the index for the start/starts of the experiment
  start_loc <- which(start == "TRUE")
  
  # get the max of the start block (remove the fals-starts)
  Start <- max(start_loc)+4
  
  # convert the indices from dataframe to numeric
  Start <- as.numeric(unlist(Start))
  
  # remove the indices from the dataset
  data<-data2[-c(1:Start),]
  
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
    if (str_detect(all_marker[i], "S")==TRUE){
      images[x]<- all_marker[i]
      x <- x+1
    }
  }
  
  # sort the vector alphabetically
  images <- sort(images)
  
  # create two empty variables for later
  f = NULL
  ff = NULL
  
  for (i in 1:length(images)){
    
    # this takes the stimulus and analyze it
    MARKER <- images[i]
    
    # this compares logically every marker with the stimulus
    INDEX <- MARKER == marker1
    
    # this saves just the logic that are equal to TRUE
    LOCATIONS <- which(INDEX == "TRUE")
    
    # we want to analyze each second after the stimulus-onset separately, and each needs 20 samples
    n_cols <- 70
    
    # we want the baseline to include the 1 seconds before the stiulus' onset, so we need 40 samples
    n_cols_baseline = 20
    
    # create an empty variable for later
    d =NULL
    dd= NULL
    
    # get for all the images presentations, the baseline-corrected average for the 5 seconds following the images onset
    
    for (z in 1:length(LOCATIONS)){
      
      # get the specifc index for the n-stimulus presentation
      LOC <- LOCATIONS[z]
      
      # get the average response for the 2 seconds before the stimulus onset [i.e., baseline]
      Baseline <- mean(values[seq(to = LOC, length.out = n_cols_baseline)],na.rm = TRUE)
      
      # if US then get the max response for the 6.5-10 second interval after stimulus onset and then subtract the baseline
      After_US <- max(values[seq(from = (LOC+130), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # if CS then get the max respose for the 0.5-4 second interval after stimulus onset and then subtract the baseline
      After_CS <- max(values[seq(from = (LOC+10), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # combine the vectors into a dataframe (second 1st to 6th)
      trial <- After_US
      
      # combine the vectors into a dataframe (second 6th to 12th)
      trial2 <- After_CS
      
      # if the value is below 0.01 we replace it with a NA
      if ((trial < 0.01)==TRUE){
        trial<- NA
      }
      
      # if the value is below 0.01 we replace it with a NA
      if ((trial2 < 0.01)==TRUE){
        trial2<- NA
      }
      
      # convert it into a dataframe
      trial <- as.data.frame(trial)
      
      # convert it into a dataframe
      trial2 <- as.data.frame(trial2)
      
      # rename the columns
      colnames(trial) <- paste(substring(MARKER,4,nchar(MARKER)-1),"T",z,"-US", sep="")
      
      # rename the columns
      colnames(trial2) <- paste(substring(MARKER,4,nchar(MARKER)-1),"T",z,"-CS", sep="")
      
      # save values outside the for-loop (1st iteration)
      if ((z==1)==TRUE){
        
        # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
        d<- rbind(d, trial)
        dd <- rbind(dd, trial2)
        
        # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
        d_remove <- length(d)
        dd_remove <- length(dd)
      }
      
      # save the values outside the for-loop (2nd iteration and onwards)
      d<- cbind(d, trial)
      dd <- cbind(dd, trial2)
      
      # empty the variable for the next iteration
      rm(trial, trial2)
    }
    
    # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
    d <- d[, -c(1:d_remove)]
    dd <- dd[, -c(1:dd_remove)]
    
    if ((i ==1)==TRUE){
      
      # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
      f <- rbind(f,d)
      ff <- rbind(ff,dd)
      
      # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
      col_remove <- length(d)
      col_remove2 <- length(dd)
    }
    
    # save the values outside the for-loop
    f<- cbind(f,d)
    ff <- cbind(ff,dd)
    
    # empty the variable for the next iteration
    rm(d,dd)
  }
  
  # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
  f <- f[, -c(1:col_remove)]
  ff <- ff[, -c(1:col_remove2)]
  
  # get the id 
  id <- as.factor(substring(files[y],13,14))
  
  # add the id name to the dataset
  f <- cbind(id,f)
  ff<- cbind(id, ff)
  
  # save the values outside the for-loop
  FINAL <-rbind(FINAL, f)
  FINAL2 <- rbind(FINAL2, ff)
}
# replace the NA with 0
FINAL[is.na(FINAL)]<-0

# replace the NA with 0
FINAL2[is.na(FINAL2)]<-0

# save the dataset as a csv file
write.csv(FINAL, file = "REW-PEAK-US.csv")
write.csv(FINAL2, file = "REW-PEAK-CS.csv")
