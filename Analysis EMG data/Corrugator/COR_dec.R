## THIS SCRIPT ANALYSISE THE EMG DATA FOR THE CORRUGATOR FOR THE DECK_TASK
## BEFORE RUNNING THE SCRIPT CHECK THE SAMPLING RATE OF THE EMG DATA [AT THE MOMENT THE INTERVAL IS 0.05, SO 20 SAMPLES MAKE A SECOND]
## THIS SCRIPT PRODUCES 2 CSV FILES: ONE FOR THE FEEDBACK VERSION OF THE DEC_TASK (I.E., "COR_FINAL_feed.csv") AND ONE FOR THE NO-FEEDBACK VERSION OF THE TASK

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
  setwd("C:/Users/zj903545/OneDrive - University of Reading/Jayne/dec_task/")
} else{
  # MAC
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/dec_task/")
}

# get the list of all the .txt files in the folder
files_txt <- list.files(pattern = ".txt")

# this removes all the .txt files that are not relevant
y=1
files<-vector()
for (i in 1:length(files_txt)){
  if (str_detect(files_txt[i], "dec_temp")==TRUE){
    files[y]<- files_txt[i]
    y<-y+1
  }
}

# create teo empty variables for later
FINAL =NULL
FINAL_feed = NULL

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
  
  # get the max of the start block (remove the fals-starts)
  Start <- max(start_loc)+3
  
  # convert the indices from dataframe to numeric
  Start <- as.numeric(unlist(Start))
  
  # remove the indices from the dataset
  data<-data[-c(1:Start),]
  
  # convert the file in a data frame
  data <- as.data.frame(data)
  
  # the first column contains the time stamp info
  time <- data$V1
  
  # the second column contain the EMG scores
  values <- data$V2
  
  # convert the GSR values to numeric
  values<-as.numeric(as.character(values))
  
  # change the names of the markers
  levels(data$V3)[levels(data$V3)=="#* KNstart "]<- "#* c0 "
  levels(data$V3)[levels(data$V3)=="#* KPstart "]<- "#* c1 "
  levels(data$V3)[levels(data$V3)=="#* KUstart "]<- "#* c2 "
  levels(data$V3)[levels(data$V3)=="#* UTstart "]<- "#* c3 "
  levels(data$V3)[levels(data$V3)=="#* KNcue "]<- "#* c4 "
  levels(data$V3)[levels(data$V3)=="#* KPcue "]<- "#* c5 "
  levels(data$V3)[levels(data$V3)=="#* KUcue "]<- "#* c6 "
  levels(data$V3)[levels(data$V3)=="#* UTcue "]<- "#* feed "
  
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
    if (str_detect(all_marker[i], "#")==TRUE){
      images[x]<- all_marker[i]
      x <- x+1
    }
  }
  
  # sort the vector alphabetically
  images <- sort(images)
  
  # create two empty variables for later
  f = NULL
  ff = NULL
  
  # this is the loop for the no-feedback version of the dec_task
  if ((length(images)==7)==TRUE){
    
    for (i in 1:length(images)){
      
      # this takes the image and analyze it
      MARKER <- images[i]
      
      # this compares logically every marker with the image
      INDEX <- MARKER == marker1
      
      # this saves just the logic that are equal to TRUE
      LOCATIONS <- which(INDEX == "TRUE")
      
      # we want to analyze each second after the image-onset separately, and each needs 1000 samples
      n_cols <- 20
      n_cols_baseline <-40
      
      # create an empty variable for later
      d =NULL
      
      # get for all the images presentations, the baseline-corrected average for the 5 seconds following the images onset
      for (z in 1:length(LOCATIONS)){
        
        # get the specifc index for the n-image presentation
        LOC <- LOCATIONS[z]
        
        # get the average response for the two seconds before the image onset [i.e., baseline]
        Baseline <- mean(values[seq(to = LOC, length.out = n_cols_baseline)],na.rm = TRUE)
        
        # get the average response for the 1st second after the image onset and then subtract the baseline
        After1 <- mean(values[seq(from = LOC, length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 2nd second after the image onset and then subtract the baseline
        After2 <- mean(values[seq(from = (LOC+20), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 3rd second after the image onset and then subtract the baseline
        After3 <- mean(values[seq(from = (LOC+40), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 4st second after the image onset and then subtract the baseline
        After4 <- mean(values[seq(from = (LOC+60), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # combine the vectors into a dataframe
        trial <- cbind(After1, After2, After3, After4)
        
        # convert it into a dataframe
        trial <- as.data.frame(trial)
        
        # rename the columns
        colnames(trial) <- c(paste(substring(MARKER,4,5),"T",z,"S1", sep=""), paste(substring(MARKER,4,5),"T",z,"S2", sep=""), paste(substring(MARKER,4,5),"T",z,"S3", sep=""), paste(substring(MARKER,4,5),"T",z,"S4", sep=""))
        
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
    id <- as.factor(substring(files[y],14,15))
    
    # add the id name to the dataset
    f <- cbind(id,f)
    
    # convert it to a data frame
    f <- as.data.frame(f)
    
    # save the values outside the for-loop
    FINAL <-rbind(FINAL, f)
    
    # this is the loop for the feedback version of the dec_task
  }else if ((length(images)==8)==TRUE){
    
    for (i in 1:length(images)){
      
      # this takes the image and analyze it
      MARKER <- images[i]
      
      # this compares logically every marker with the image
      INDEX <- MARKER == marker1
      
      # this saves just the logic that are equal to TRUE
      LOCATIONS <- which(INDEX == "TRUE")
      
      # we want to analyze each second after the image-onset separately, and each needs 1000 samples
      n_cols <- 20
      n_cols_baseline <-40
      
      # for the feedback marker we want just one second baseline
      if ((MARKER=="#* feed ")==TRUE){
        n_cols_baseline<-20
      }
      
      # create an empty variable for later
      dd =NULL
      
      # get for all the images presentations, the baseline-corrected average for the 5 seconds following the images onset
      
      for (z in 1:length(LOCATIONS)){
        
        # get the specifc index for the n-image presentation
        LOC <- LOCATIONS[z]
        
        # get the average response for the second before the image onset [i.e., baseline]
        Baseline <- mean(values[seq(to = LOC, length.out = n_cols_baseline)],na.rm = TRUE)
        
        # get the average response for the 1st second after the image onset and then subtract the baseline
        After1 <- mean(values[seq(from = LOC, length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 2nd second after the image onset and then subtract the baseline
        After2 <- mean(values[seq(from = (LOC+20), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 3rd second after the image onset and then subtract the baseline
        After3 <- mean(values[seq(from = (LOC+40), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # get the average response for the 4st second after the image onset and then subtract the baseline
        After4 <- mean(values[seq(from = (LOC+60), length.out = n_cols)],na.rm = TRUE) - Baseline
        
        # combine the vectors into a dataframe
        trial <- cbind(After1, After2, After3, After4)
        
        # convert it into a dataframe
        trial <- as.data.frame(trial)
        
        # rename the columns
        colnames(trial) <- c(paste(substring(MARKER,4,5),"T",z,"S1", sep=""), paste(substring(MARKER,4,5),"T",z,"S2", sep=""), paste(substring(MARKER,4,5),"T",z,"S3", sep=""), paste(substring(MARKER,4,5),"T",z,"S4", sep=""))
        
        # use a different name for the columns related to the feedback marker
        if ((MARKER=="#* feed ")==TRUE){
          colnames(trial) <- c(paste(substring(MARKER,4,7),"T",z,"S1", sep=""), paste(substring(MARKER,4,7),"T",z,"S2", sep=""), paste(substring(MARKER,4,7),"T",z,"S3", sep=""), paste(substring(MARKER,4,7),"T",z,"S4", sep=""))
        }
        
        # save values outside the for-loop (1st iteration)
        if ((z==1)==TRUE){
          
          # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
          dd<- rbind(dd, trial)
          
          # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
          dd_remove <- length(dd)
        }
        
        # save the values outside the for-loop (2nd iteration and onwards)
        dd<- cbind(dd, trial)
        
        # empty the variable for the next iteration
        rm(trial)
      }
      
      # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
      dd <- dd[, -c(1:dd_remove)]
      
      if ((i ==1)==TRUE){
        
        # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
        ff <- rbind(ff,dd)
        
        # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
        col_remove_dd <- length(dd)
      }
      
      # save the values outside the for-loop
      ff<- cbind(ff,dd)
      
      # empty the variable for the next iteration
      rm(dd)
    }
    
    # remove the columns that are repeated twice becasue of the rbind() in the 1st iteration
    ff <- ff[, -c(1:col_remove_dd)]
    
    # get the id 
    id_feed <- as.factor(substring(files[y],14,15))
    
    # add the id name to the dataset
    ff <- cbind(id_feed,ff)
    
    # convert it to a dataframe
    ff<- as.data.frame(ff)
    
    # save the values outside the for-loop
    FINAL_feed <-rbind(FINAL_feed, ff)
  }
}

# save the no-feedback dataset as a csv file
write.csv(FINAL, file = "COR_FINAL.csv")

# save the feedback dataset as a csv file
write.csv(FINAL_feed, file = "COR_FINAL_feed.csv")
