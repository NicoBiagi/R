## THIS SCRIPT ANALYSISE THE GSR DATA.
## BEFORE RUNNING THE SCRIPT CHECK THE SAMPLING RATE OF THE GSR DATA [AT THE MOMENT THE INTERVAL IS 0.001, SO 1000 SAMPLES MAKE A SECOND] 

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
  setwd("C:/Users/zj903545/OneDrive - University of Reading/Jayne/ThreatOfShock/data_threatofshock/")
} else{
  # MAC
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/ThreatOfShock/data_threatofshock/")
}

# get the list of all the .txt files in the folder
files_txt <- list.files(pattern = ".txt")

# this removes all the .txt files that are not relevant
y=1
files<-vector()
for (i in 1:length(files_txt)){
  if (str_detect(files_txt[i], "AS")==TRUE){
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
  
  # get teh index for the start/starts of the experiment
  start_loc <- which(start == "TRUE")
  
  # create an empty variable for later
  Start = NULL
  
  # get the max value of start (i.e, the last time the task was started)
  max_start <- max(start_loc)
  
  # add to the value the 7 rows that we want to remove
  Start <- max_start + 7
  
  # remove the indices from the dataset
  data<-data[-c(1:Start),]
  
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

  # make sure that we have just one type of marker for the AloneSafe
  for (x in 1:length(marker1)){
    if (str_detect(marker1[x], "Safe")==TRUE){
      marker1[x]<- "AloneSafe"
    }
  }
  
  # make sure that we have just one type of marker for the AloneThreat
  for (x in 1:length(marker1)){
    if (str_detect(marker1[x], "Threat")==TRUE){
      marker1[x]<- "AloneThreat"
    }
  }
  
  # get the unique characters in the marker variable
  all_marker <- unique(marker1)
  
  # this removes all the markers that are not relevant 
  x=1
  images <- vector()
  for (i  in 1:length(all_marker)){
    if (str_detect(all_marker[i], "Alone")==TRUE){
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
    
    # select the name of the coulmn in the dataframe based on the stimulus name
    if (str_detect(MARKER, "Threat")==TRUE){
      NAME = "AloneThreat"
    }else{
      NAME = "AloneSafe"
    }
    
    # this compares logically every marker with the image
    INDEX <- MARKER == marker1
    
    # this saves just the logic that are equal to TRUE
    LOCATIONS <- which(INDEX == "TRUE")
    
    # we want the baseline to be 1 second, therefore we need 50 rows (i.e., smpling rate is 50 Hz)
    n_cols_baseline <- 50
    
    # we want the peak response in the 0.5-9 seconds window following stim onset
    n_cols <- 450
    
    # create an empty variable for later
    d =NULL
    
    for (z in 1:length(LOCATIONS)){
      # get the specifc index for the n-image presentation
      LOC <- LOCATIONS[z]
      
      # get the average response for the second before the image onset [i.e., baseline]
      Baseline <- mean(values[seq(to = LOC, length.out = n_cols_baseline)],na.rm = TRUE)
      
      # get the max response for the 0.5-9 seconds interval after the image onset and then subtract the baseline
      Max_after <- max(values[seq(from = (LOC+25), length.out = n_cols)],na.rm = TRUE) - Baseline
      
      # if the value is below 0.01 we replace it with a NA
      if ((Max_after < 0.01)==TRUE){
        Max_after<- NA
      }
      
      # combine the vectors into a dataframe
      trial <- Max_after
      
      # convert it into a dataframe
      trial <- as.data.frame(trial)
      
      # rename the columns
      colnames(trial) <- paste(NAME,"T",z, sep="")
      
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
# replace the NA with 0
FINAL[is.na(FINAL)]<-0

# save the dataset as a csv file
write.csv(FINAL, file = "ToS_MAX.csv")

