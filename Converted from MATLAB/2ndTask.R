# remove all the variables from the global environment
rm(list = ls())
library("eyelinker")
library("tidyverse")
library("tidyr")
library("intervals")

# turn this off if you want scientific notation
options(scipen = 999)

if (Sys.info()[[1]] == "Windows"){
  setwd('C://Users/zj903545/OneDrive - University of Reading/PhD/Undergraduate Project/Kate & Karen/Eyetracker/')
}else{
  setwd('/Users/nico/OneDrive - University of Reading/PhD/Undergraduate Project/Kate & Karen/Eyetracker/')
}

# get the list of all the .txt files in the folder
files <- list.files(pattern = ".asc")
 d= NULL

for (i in 1:length(files)){
  dat<-read.asc(files[i])
  print(files[i])
  Id <- files[i]
  
  
  # remove the column 6 [it's empty]
  dat$raw <- dat$raw[,-6]
  
  # get just the raw eyetracker data
  raw <- dat$raw
  
  # tidy up the data
  raw.long <- dplyr::select(raw,time,xp,yp,block) %>% gather("coord","pos",xp,yp)
  
  raw.long <- mutate(raw.long,ts=(time-min(time))/1e3) #let's have time in sec. 
  
  
  # get the saccades
  sac <- dat$sac
  
  msg <- dat$msg
  
  if (str_detect(Id, "E_")==TRUE){
    # get the the time stamp when the dots were presented on the screen
    START <-msg[ which(msg$text == "MULLERON"),1]
    
    # get the the time stamp when the dots were presented on the screen
    END<-msg[ which(msg$text == "MULLEROFF"),1]
    
    # combine the two vectors together
    time_window <- cbind(START, END)
    
    
  }else{
    # get the the time stamp when the dots were presented on the screen
    START<- msg[ which(msg$text == "DOTSON"),1]
    
    #get the time stamp when the trial ended
    END <- msg[ which(msg$text == "TRIAL_RESULT 0"),1]
    
    # combine the two vectors together
    time_window <- cbind(START, END)
  }
  
  # for the raw data check whether or not they were made during between the dots presentation and the end of the relative trial
  raw <- mutate(raw, saccade= time %In% time_window)
  
  # for all the saccade check whether or not they were made during between the dots presentation and the end of the relative trial
  sac <- mutate(sac, saccade = stime %In% time_window)
  
  # get the trial number
  n_row <- 1:nrow(time_window)
  
  # combine the the trial number with the time-stamps for the dots onset and the trial end
  time_window <- cbind(n_row, time_window)
  
  # convert it to a data frame
  time_window <- as.data.frame(time_window)#
  
  # add a column at the end of the sac dataframe composed of just 'NA'
  sac[,(ncol(sac)+1)]<-NA
  
  # rename the column of the dataframe as trial
  colnames(sac)[13]<- "trial"
  
  # for each saccade in the 'sac' data frame, get the trial in which the saccade was made
  for (i in 1:nrow(sac)){
    for (y in 1:nrow(time_window)){
      
      # if the saccade onset is bigger than the DOTS onset and the saccade offset is smaller than the end of the trial,
      # then the saccade was made in that trial. If not, we need to check if the saccade was made in the next trial
      if ((sac$stime[i] > time_window$START[y] && sac$etime[i] < time_window$END[y])==FALSE) {
        y<-y+1
        
        
      }else{
        
        sac$trial[i] <- time_window$n_row[y]
        y<-1
        
      }
    }
  }
  
  sac<-na.omit(sac)
  if (str_detect(Id, "B_")==TRUE){
    Task <-1
  }else if (str_detect(Id, "E_")==TRUE){
    Task<-2
  }else if (str_detect(Id, "NI_")==TRUE){
    Task<-3
  }
  
  Task <- as.data.frame(as.double(t(rep(Task, length.out = nrow(sac)))))
  names(Task) <- "Task"
  
  
  if (nchar(Id)==11){
    ID <- str_sub(Id, 3,4)
  }else{
    ID <- str_sub(Id, 4,5)
  }
  ID <- as.data.frame(as.double(t(rep(ID, length.out = nrow(sac)))))
  names(ID)<- "ID"
  
  
  Session <- str_sub(Id, -5,-5)
  SES <- as.data.frame(as.double(t(rep(Session, length.out = nrow(sac)))))
  names(SES)<- "Session"
  
  sac <- cbind(ID, SES, sac, Task)
  d <- rbind(d, sac)
  filename <- str_sub(Id, 1, -5)
  
  
  # write.csv(sac, file = paste(filename, ".csv"))

  
  # rm(list=setdiff(ls(), c("files","i", "d")))
}
write.csv(d, file = "OUTPUT.csv")

