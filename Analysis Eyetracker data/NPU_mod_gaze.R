## THIS SCRIPT ANALYSISE THE EYETRACKER DATA FOR THE NPU TASK.

# remove all the variables from the global environment
rm(list = ls())

# load some libraries
library("tidyverse")
library("tidyr")
library("intervals")
library("readxl")
library("data.table")

# turn this off if you want scientific notation
options(scipen = 999)

# set the working directory
if (Sys.info()[[1]] == "Windows"){
  setwd('C://Users/zj903545/OneDrive - University of Reading/Jayne/NPU_mod_Gazedata')
}else{
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/NPU_mod_Gazedata") #mac
}
# get the list of all the .txt files in the folder
files <- list.files(pattern = ".gazedata")

# create an empty variable for later
FINAL =NULL

#this creates a loop for all the partecipants
for (y in 1:length(files)){
  
  # this prints the filename
  print(files[y])
  
  # this gets the participant's id 
  id <- as.numeric(substring(files[y],11,12))
  
  # read the .txt file
  data <- read.delim(files[y], header=FALSE, stringsAsFactors = FALSE)
  
  # use the first row of the data frame as columns names
  colnames(data) <- data[1,]
  
  # remove the first row from the data frame
  data <- data[-1, ]
  
  
  data$CurrentObject[data$CurrentObject == "NBeforeCue"] <- "KN"
  
  data$CurrentObject[data$CurrentObject == "RBeforeCue"] <- "UT"
  data$CurrentObject[data$CurrentObject == "R1BeforeCue"] <- "UT1"
  data$CurrentObject[data$CurrentObject == "R2BeforeCue"] <- "UT2"
  
  data$CurrentObject[data$CurrentObject == "PSBeforeCue"] <- "KP-SHOCK"
  data$CurrentObject[data$CurrentObject == "PNOSBeforeCue"] <- "KP-NOSHOCK"
  
  data$CurrentObject[data$CurrentObject == "UBeforeCue"] <- "KU-NOSHOCK"
  data$CurrentObject[data$CurrentObject == "UPFBeforeCue"] <- "KU-PROBEFIRST"
  data$CurrentObject[data$CurrentObject == "USFBeforeCue"] <- "KU-SHOCKFIRST"
  
  eye_data <- select(data, Subject, TETTime, RTTime, TimestampMicrosec, DiameterPupilLeftEye, DiameterPupilRightEye, TrialId, CurrentObject)
  
  
  all_marker <- unique(eye_data$CurrentObject)
  
  # this removes all the markers that are not relevant to the sound onset
  x=1
  cue <- vector()
  for (i  in 1:length(all_marker)){
    if (str_detect(all_marker[i], "KN")==TRUE | str_detect(all_marker[i], "UT" )==TRUE | str_detect(all_marker[i], "KP" )==TRUE | str_detect(all_marker[i], "KU" )==TRUE){
      cue[x]<- all_marker[i]
      x <- x+1
    }
  }
  
  # sort the vector alphabetically
  cue <- sort(cue)
  
  # sampling rate of the eye-tracker
  n_rows <- 300
  
  f = NULL
  
  for (o in 1:length(cue)){
    
    MARKER <- cue[o]
    
    subset <- eye_data[eye_data$CurrentObject==MARKER,]
    
    trials <- unique(subset$TrialId)
    
    fuffa = NULL
    
    Count <- 1
    
    for (z in 1:length(trials)){
      
      TRIAL <- trials[z]
      
      INDEX <- MARKER == eye_data$CurrentObject & TRIAL==eye_data$TrialId
      
      LOC <- which(INDEX == "TRUE")
      
      if ((length(LOC)==0)==FALSE){
        
        MIN <- min(LOC)
        
        Baseline <- select(eye_data[seq(to = MIN, length.out = n_rows),], starts_with('DiameterPupil'))
        Baseline <- as.data.frame(sapply(Baseline, as.numeric))
        Baseline <- Baseline[!Baseline$DiameterPupilLeftEye == -1, ]
        Baseline.AVG <- mean(rowMeans(Baseline), na.rm = TRUE)
        
        delay <- 0
        
        resp = NULL
        
        for (tt in 1:6){
          
          After1 <- select(eye_data[seq(from = MIN+delay, length.out = n_rows),], starts_with('DiameterPupil'))
          After1 <- as.data.frame(sapply(After1, as.numeric))
          After1 <- After1[!After1$DiameterPupilLeftEye == -1, ]
          After1.AVG <- mean(rowMeans(After1), na.rm = TRUE)
          
          
          EYE_RESP <- After1.AVG - Baseline.AVG
          
          # this converts the baseline corrected response into a dataframe
          EYE_RESP <- as.data.frame(EYE_RESP)
          
          # this renames the column
          colnames(EYE_RESP)<-paste(MARKER,"trial", Count, "sec", tt, sep = "-")
          
          if ((tt ==1)==TRUE){
            
            resp <- rbind(EYE_RESP, resp)
            
          }else{
            
            resp <- cbind(resp,EYE_RESP)
          }
          
          delay <- delay +201
          
          rm(EYE_RESP)
        }
        
        
      }else{
        # this converts the baseline corrected response into a dataframe
        resp <- data.frame(matrix(NA, nrow = 1, ncol = 6))
        
        # this renames the column
        colnames(resp)<-c(paste(MARKER,"trial", Count, "sec", 1, sep = "-"),paste(MARKER,"trial", Count, "sec", 2, sep = "-"),paste(MARKER,"trial", Count, "sec", 3, sep = "-"),paste(MARKER,"trial", TRIAL, "sec", 4, sep = "-"),paste(MARKER,"trial", Count, "sec", 5, sep = "-"),paste(MARKER,"trial", Count, "sec", 6, sep = "-"))
      }
      
      if ((length(fuffa)==0)==TRUE){
        
        fuffa <- rbind(resp, fuffa)
        
      }else{
        
        fuffa <- cbind(fuffa, resp)
      }
      Count <- Count + 1
    }
    
    if ((length(f)==0)==TRUE){
      
      f <- rbind(fuffa, f)
    }else{
      
      f <- cbind(f, fuffa)
    }
  }
  
  # add the id name to the dataset
  f <- cbind(id,f)
  
  FINAL <- rbind(FINAL, f)
  rm(f)
}
write.csv(FINAL, file = "NPU_mod_gaze_FINAL.csv")

