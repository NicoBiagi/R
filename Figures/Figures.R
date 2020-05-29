## THIS SCRIPT CREATES SOME GRAPHS FOR THE NPU TASK
# remove all the variables from the global environment
rm(list = ls())

# load some libraries
library("tidyverse")
library("tidyr")
library("intervals")
library("readxl")
library("data.table")
library("yarrr")
library("ggplot2")
library("extrafont")
font_import()
loadfonts(device="win")
fonts()    

# turn this off if you want scientific notation
options(scipen = 999)

# set the working directory
if (Sys.info()[[1]] == "Windows"){
  setwd('C://Users/zj903545/OneDrive - University of Reading/Jayne/')
}else{
  setwd("/Users/nico/OneDrive - University of Reading/Jayne/") #mac
}

# load the NPU data file in the long format
data <- read_excel("NPU_R_Long.xlsx")

# load the STARTLE data file
data_startle <- read_excel("STARTLE.xlsx")

# load the questionnaire data file
questionnaires <- read_excel("Questionnaires.xlsx")

# from all the questionnaires we will save just the IU and the STICSA
dat_quest <- questionnaires[questionnaires$Questionnarie == "IU"| questionnaires$Questionnarie == "STICSA",]

# create a graph for the average of arousal
ggsave("Arousal.jpeg", plot = pirateplot(formula = Arousal_AVG~Arousal,
                                         data = data,
                                         main = "",
                                         pal = "southpark", # southpark color palette
                                         bean.f.o = .6, # Bean fill
                                         ylab = "Arousal rating",
                                         xlab = "",
                                         point.o = .8, # Points
                                         inf.f.o = .7, # Inference fill
                                         inf.b.o = .8, # Inference border
                                         avg.line.o = 1, # Average line
                                         inf.f.col = "white", # Inf fill col
                                         inf.b.col = "black", # Inf border col
                                         avg.line.col = "black", # avg line col
                                         point.cex = .7,
                                         cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the average of valence
ggsave("Valence.jpeg", plot = pirateplot(formula = Valence_AVG~Valence,
                                         data = data,
                                         main = "",
                                         pal = "southpark", # southpark color palette
                                         bean.f.o = .6, # Bean fill
                                         ylab = "Valence rating",
                                         xlab = "",
                                         point.o = .8, # Points
                                         inf.f.o = .7, # Inference fill
                                         inf.b.o = .8, # Inference border
                                         avg.line.o = 1, # Average line
                                         inf.f.col = "white", # Inf fill col
                                         inf.b.col = "black", # Inf border col
                                         avg.line.col = "black", # avg line col
                                         point.cex = .7,
                                         cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the average of SCR
ggsave("SCR.jpeg", plot = pirateplot(formula = SCR_AVG~SCR,
                                     data = data,
                                     main = "",
                                     pal = "southpark", # southpark color palette
                                     bean.f.o = .6, # Bean fill
                                     ylab = "Square root transformed and z-scored SCR magnitude (?????S)",
                                     xlab = "",
                                     point.o = .8, # Points
                                     inf.f.o = .7, # Inference fill
                                     inf.b.o = .8, # Inference border
                                     avg.line.o = 1, # Average line
                                     inf.f.col = "white", # Inf fill col
                                     inf.b.col = "black", # Inf border col
                                     avg.line.col = "black", # avg line col
                                     point.cex = .7,
                                     cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the average of CORR
ggsave("CORR.jpeg", plot = pirateplot(formula = CORR_AVG~CORR,
                                      data = data,
                                      main = "",
                                      pal = "southpark", # southpark color palette
                                      bean.f.o = .6, # Bean fill
                                      ylab = "Z-scored corrugator supercilii activity (??V)",
                                      xlab = "",
                                      point.o = .8, # Points
                                      inf.f.o = .7, # Inference fill
                                      inf.b.o = .8, # Inference border
                                      avg.line.o = 1, # Average line
                                      inf.f.col = "white", # Inf fill col
                                      inf.b.col = "black", # Inf border col
                                      avg.line.col = "black", # avg line col
                                      point.cex = .7,
                                      cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the average of Startle
ggsave("Startle-CUE.jpeg", plot = pirateplot(formula = STARTLE_CUE_AVG~STARTLE_CUE,
                                         data = data,
                                         main = "",
                                         pal = "southpark", # southpark color palette
                                         bean.f.o = .6, # Bean fill
                                         ylab = "Z-scored orbicularis oculi activity (??V)",
                                         xlab = "",
                                         point.o = .8, # Points
                                         inf.f.o = .7, # Inference fill
                                         inf.b.o = .8, # Inference border
                                         avg.line.o = 1, # Average line
                                         inf.f.col = "white", # Inf fill col
                                         inf.b.col = "black", # Inf border col
                                         avg.line.col = "black", # avg line col
                                         point.cex = .7,
                                         cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the average of Startle
ggsave("Startle-ISI.jpeg", plot = pirateplot(formula = STARTLE_ISI_AVG~STARTLE_ISI,
                                             data = data,
                                             main = "",
                                             pal = "southpark", # southpark color palette
                                             bean.f.o = .6, # Bean fill
                                             ylab = "Z-scored orbicularis oculi activity (??V)",
                                             xlab = "",
                                             point.o = .8, # Points
                                             inf.f.o = .7, # Inference fill
                                             inf.b.o = .8, # Inference border
                                             avg.line.o = 1, # Average line
                                             inf.f.col = "white", # Inf fill col
                                             inf.b.col = "black", # Inf border col
                                             avg.line.col = "black", # avg line col
                                             point.cex = .7,
                                             cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")

# create a graph for the Pupil's dialtion 
ggsave("Pupil.jpeg", plot = pirateplot(formula = PD_AVG~PD,
                                             data = data,
                                             main = "",
                                             pal = "southpark", # southpark color palette
                                             bean.f.o = .6, # Bean fill
                                             ylab = "Z-scored pupil dilation (??mm)",
                                             xlab = "",
                                             point.o = .8, # Points
                                             inf.f.o = .7, # Inference fill
                                             inf.b.o = .8, # Inference border
                                             avg.line.o = 1, # Average line
                                             inf.f.col = "white", # Inf fill col
                                             inf.b.col = "black", # Inf border col
                                             avg.line.col = "black", # avg line col
                                             point.cex = .7,
                                             cex.names=.65),
       width = 20, height = 15, units = "cm", dpi = "retina")


# create a double-histogram for both the IU and the STICSA
p <- ggplot(dat_quest, aes(x=Score, fill=Questionnarie)) +
  geom_histogram(position="identity", colour="black", alpha=0.2, bins = 10) 
p+  facet_grid( .~Questionnarie, scales="free")+theme(strip.text.y = element_blank())

ggsave("Questionnaires.jpeg", plot = p, width = 20, height = 15, units = "cm", dpi = "retina")
