## THIS SCRIPT ANALYSISE THE EMG DATA FOR THE OBICULARIS MUSCLE.
## BEFORE RUNNING THE SCRIPT CHECK THE SAMPLING RATE OF THE EMG DATA [AT THE MOMENT THE INTERVAL IS 0.001, SO A SAMPLE EVERY MILLISECOND, 1000 SAMPLES MAKE A SECOND] 

# this removes all the variables from the global environment
rm(list = ls())

# this loads some libraries
library("tidyverse")
library("tidyr")
library("intervals")
library("readxl")
library("data.table")
library("stringr")
library("dplyr")
library("lubridate")


# turns this off if you want scientific notation
options(scipen = 999)

# this sets the working directory
if ((Sys.info()[[1]] == "Windows")==TRUE){
  setwd('C://Users/zj903545/OneDrive - University of Reading/Jayne/NPU_mod_Orbicularis_txt/')
}else{
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/NPU_mod_Orbicularis_txt/") #mac
}

# this gets the list of all the .txt files in the folder
files_txt <- list.files(pattern = ".txt")

# this removes all the .txt files that are not relevant
y=1
files<-vector()
for (i in 1:length(files_txt)){
  if (str_detect(files_txt[i], "Orbicularis")==TRUE){
    files[y]<- files_txt[i]
    y<-y+1
  }
}

# this loads the excel file
files_exl <- list.files(pattern = ".xlsx")

# this checks if the excel file is the one with the blinks onsets
y=1
blinks_file<-vector()
for (i in 1:length(files_exl)){
  if (str_detect(files_exl[i], "Blinks")==TRUE){
    blinks_file[y]<- files_exl[i]
    y<-y+1
  }
}

# this loads the blink data from the excel file
blink_data <- read_excel(blinks_file)

# this creates an empty variable for later
FINAL =NULL

# this creates an empty variable for later
ERROR_LOG = NULL

# this creates a loop for all the partecipants
for (y in 1:length(files)){
  
  # this prints the filename
  print(files[y])
  
  # this gets the participant's id 
  id <- substring(files[y],17,18)
  
  # this gets the id as a double-digit
  if ((id<10)==TRUE){
    id <- substring(id,2,2)
  }
  
  # this reads the .txt file
  data <- read.delim(files[y], header=FALSE)
  
  # this gets all the times that the experiment was started
  start <- data[,2] == "StartOfBlock"
  
  # this gets the index for the start/starts of the experiment
  start_loc <- which(start == "TRUE")
  
  # this gets the max index value (i.e., last time that the exp was started)
  START <- max(start_loc)
  
  # this removes also the 4 rows after the start of the exp (they're also headings)
  START <- START+4
  
  # this removes the first seven rows because they don't contain any useful information
  data<-data[-c(1:START),]
  
  # the first column contains the time stamp info
  time <- data$V1
  
  # the second column contain the EMG scores
  values <- data$V2
  
  # this converts the EMG values to numeric
  values<-as.numeric(as.character(values))
  
  # the third column contains the markers
  marker <-data$V3
  
  # this converts the marker from factor to character
  marker1 <- as.character(marker)
  
  # this gets the unique characters in the marker variable
  all_marker <- unique(marker1)
  
  # this removes all the markers that are not relevant to the sound onset
  x=1
  sound <- vector()
  for (i  in 1:length(all_marker)){
    if (str_detect(all_marker[i], "ound")==TRUE){
      sound[x]<- all_marker[i]
      x <- x+1
    }
  }
  
  # this sorts the vector alphabetically
  sound <- sort(sound)
  
  # this creates an empty variable for later
  f = NULL
  
  # this creates an empty variable for later
  p = NULL
  
  # this creates an empty variable for later
  gg = NULL
  
  # this creates a loop for all the sounds type
  for (i in 1:length(sound)){
    
    # this selects one sound-marker
    MARKER <- sound[i]
    
    # this selects the relevant rows from the blink-onset data
    sub_blink <- blink_data[which(blink_data$Marker == substring(MARKER,4,nchar(MARKER)-1)& blink_data$Sub == id),9]
    
    if((nrow(sub_blink)==0)==TRUE){
      sub_blink<-blink_data[which(blink_data$Sub == 01),8]
      sub_blink<- unique(sub_blink)
      colnames(sub_blink)<- "Time_MM"
    }
    
    sub_blink$Time_MM<- as.numeric(sub_blink$Time_MM)
    
    # this finds all the times that sound-marker was presented
    INDEX <- MARKER == marker1
    LOCATIONS <- which(INDEX == "TRUE")
    
    # for the baseline we will consider the EMG values for 25 ms before sound onset (i.e., 25 timestamps)
    n_cols_baseline <- 25
    
    # for the response we will consider the EMG values for the time window 20-120 ms after stimulus onset (100 ms)
    n_cols <- 100
    
    # this creates an empty variable for later
    d =NULL
    
    # this creates an empty variable for later
    g = NULL
    
    # this puts the values in the empty matrixes
    for (z in 1:length(LOCATIONS)){
      
      # get the specifc index for the n-image presentation
      LOC <- LOCATIONS[z]
      
      # this tells us if a blink-onset was manually detected around the time of the sound onset
      q=1
      
      # this creates an empty vector (i.e., length = 0)
      ERROR <- vector()
      for (t in 1:nrow(sub_blink)){
        # this generates a sequence of 10 ms around the manually detected blink-onset
        around<- seq(from = sub_blink$Time_MM[t]-10, to = sub_blink$Time_MM[t]+10, by = 1)
        # this checks if the sound onset is in the sequence
        if((LOC %in% around)==TRUE){
          # in case the sound onset falls within the sequence, we change the length of the empty vector called ERROR
          ERROR[q] <- sub_blink$Time_MM[t]
          # q <- q+1
        }
      }
      
      # this checks that the length of the vector ERROR is still 0 (i.e., no manually detected blinks around the sound onset)
      if ((length(ERROR)==1)==FALSE){
        
        # this gets the average response for the second before the image onset [i.e., baseline]
        Baseline.AVG <- values[seq(to = LOC, length.out = n_cols_baseline)]
        
        # this checks if the participant had blinked during the baseline
        if ((max(Baseline.AVG) > 40) ==TRUE) {
          
          # if they had blinked then this removes the trial from the analysis
          EMG_BC <- NA
          
          error <- cbind(id, paste(substring(MARKER,4), z, sep = ""),LOC, ERROR, "BASELINE")
          
        }else{
          
          # this calculates the mean for the basline
          Baseline.AVG <- mean(Baseline.AVG, na.rm = TRUE)
          
          # this gets the max response for the time window 20-120 ms after sound onset
          After1.Max <- max(values[seq(from = LOC+20, length.out = n_cols)],na.rm = TRUE) 
          
          # this computes the baseline correction
          EMG_BC <- After1.Max - Baseline.AVG
          
          # this checks if the response is negative, and if so replaces it with a 0
          if ((EMG_BC<0)==TRUE){
            EMG_BC<-0
          }
          
        }
      # in case the length of the vector ERROR is not 0 (i.e., there was a blink manually detect around the sound onset) this trial is not analysed
      }else{
        EMG_BC <- NA
        
        error <- cbind(id, paste(substring(MARKER,4), z, sep = ""),LOC, ERROR, "MANUAL")
      }
      
      # this converts the baseline corrected response into a dataframe
      EMG_BC <- as.data.frame(EMG_BC)
      
      # this renames the column
      colnames(EMG_BC)<-paste(substring(MARKER,4), z, sep = "")
      
      # this saves values outside the for-loop (1st iteration)
      if ((z==1)==TRUE){
        
        # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
        d<- rbind(d, EMG_BC)
        
        # get the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
        d_remove <- length(d)
      }
      
      # this saves the values outside the for-loop (2nd iteration and onwards)
      d<- cbind(d, EMG_BC)
      
      if ((length(ERROR)==1)==TRUE){
      g <-rbind(g, error)
      }
      
      # this empties the variable for the next iteration
      rm(EMG_BC)
    }
    
    # this removes the columns that are repeated twice becasue of the rbind() in the 1st iteration
    d <- d[, -c(1:d_remove)]
    
    # this calculates the percentage of NAs for this condition (i.e., trials not analysed)
    na_perc <- rowMeans(is.na(d[,-1])) *100
    
    # this converts the number to a data frame
    na_perc <- as.data.frame(na_perc)
    
    # this renames the column
    colnames(na_perc) <- paste(substring(MARKER,4,nchar(MARKER)-1),"NApercentage",sep="-")
    
    # this saves values outside the for-loop (1st iteration)
    if ((i ==1)==TRUE){
      
      # cannot do a cbind() on an empty variables, so I have to do rbind(), which means that the columns saved to the new variable from the first iteration will be saved twice
      f <- rbind(f,d)
      
      # cannot do a cbind() on an empty variables, so I have to do rbind()
      p<- rbind(p,na_perc)
      
      # this gets the number of columns of the dataframe for the 1st itearation since they'll be duplicated in the final dataframe and we need to remove them
      col_remove <- length(d)
    }
    
    # this saves the values outside the for-loop(2nd iteration and onwards)
    f<- cbind(f,d)
    
    # this saves the values outside the for-loop(2nd iteration and onwards)
    p<- cbind(p, na_perc)
    
    gg <- rbind(gg,g)
    
    # this empties the variable for the next iteration
    rm(d, na_perc)
  }
  
  # this removes the columns that are repeated twice becasue of the rbind() in the 1st iteration
  f <- f[, -c(1:col_remove)]
  
  # this removes the columns that are repeated twice becasue of the rbind() in the 1st iteration
  p <- p[,-1]
  
  # get the id 
  id <- as.factor(id)
  
  # add the id name to the dataset
  f <- cbind(id,f, p)
  
  # save the values outside the for-loop
  FINAL <-rbind(FINAL, f)
  
  ERROR_LOG<- rbind(ERROR_LOG, gg)
}

# save the dataset as a csv file
write.csv(FINAL, file = "NPU_Orbicularis_FINAL.csv")
colnames(ERROR_LOG) <- c("ID", "MARKER", "DATA_BLINK", "MANUAL_BLINK","DEC_TYPE")
write.csv(ERROR_LOG, file = "ErrorBlinkLOG.csv")
