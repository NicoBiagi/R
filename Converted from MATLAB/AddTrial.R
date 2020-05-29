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

# files <- list.files(pattern = ".txt")
# d=NULL
# for (i in 1:length(files)){
#   Id <- files[i]
#   print(files[i])
#   if (str_detect(Id, "Bisection")==TRUE){
#     TASK =1
#     b<- read.table(files[i], header = FALSE, sep ="," )
#     names(b) <- c("Trial", "TrialList", "FinList", "Response", "StartX", "EndX", "NumberRight", "NumberLeft", "RandomNumber", "Offset")
#     ID <- str_sub(Id, 4, 5)
#     id <- as.data.frame(rep(ID, length.out=nrow(b)))
#     SES <- str_sub(Id, -5, -5)
#     ses <- as.data.frame(rep(SES, length.out=nrow(b)))
#     task <- as.data.frame(rep(TASK, length.out=nrow(b)))
#     a<- cbind(task, id, ses, b[,1:3])
#     colnames(a) <- c("Task","ID", "Session", "Trial", "TrialList", "FinList")
#   }else if (str_detect(Id, "Eyetracker")==TRUE){
#     TASK <- 2
#     a <- read.table(files[i], header = FALSE) 
#     a<-t(a)
#     # get the trial number
#     n_row <- 1:nrow(a)
#     fin <- as.data.frame(rep("NA", length.out = nrow(a)))
#     ID <- str_sub(Id, 4, 5)
#     id <- as.data.frame(rep(ID, length.out= nrow(a)))
#     SES <- str_sub(Id, -5, -5)
#     ses <- as.data.frame(rep(SES, length.out=nrow(a)))
#     task= as.data.frame(rep(TASK, length.out=nrow(a)))
#     a <- cbind(task, id, ses, n_row, a, fin)
#     colnames(a) <- c("Task","ID", "Session", "Trial", "TrialList", "FinList")
#   }else if (str_detect(Id, "NewIllusion")==TRUE){
#     TASK <- 3
#     b<- t(read.table(files[i], header = FALSE, sep ="" ))
#     colnames(b) <- c("Trial", "TrialList", "FinList", "Response", "StartX", "EndX", "NumberRight", "NumberLeft", "RandomNumber", "Offset")
#     ID <- str_sub(Id, 4, 5)
#     id <- as.data.frame(rep(ID, length.out=nrow(b)))
#     SES <- str_sub(Id, -5, -5)
#     ses <- as.data.frame(rep(SES, length.out=nrow(b)))
#     task <- as.data.frame(rep(TASK, length.out=nrow(b)))
#     a<- cbind(task, id, ses, b[,1:3])
#     colnames(a) <- c("Task","ID", "Session", "Trial", "TrialList", "FinList")
#     
#   }
#   d <-rbind(d, a)
#   write.csv(d, file = "TrialType.csv")
# }


# load csv file
files.csv <- list.files(pattern = ".csv")
