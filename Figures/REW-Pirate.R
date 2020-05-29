# remove all the variables from the global environment
rm(list = ls())

# load/install needed packages
if(!require(eyelinker)){install.packages("eyelinker")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(intervals)){install.packages("intervals")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(jpeg)){install.packages("jpeg")}
if(!require(readxl)){install.packages("readxl")}
if(!require(yarrr)){install.packages("yarrr")}


# turn this off if you want scientific notation
options(scipen = 999)

#wherever the script is
setwd('/Users/nico/OneDrive - University of Reading/Jayne/REW/TABLE/')

data <- read_excel("REW-Pirate.xlsx")
data2 <- read_excel("BarGraph.xlsx")

# convert it to a dataframe
data <- as.data.frame(data)

# select the distance between the bars within each condition
pd = position_dodge(1)

measure <- unique(data$Measure)

measure_bar <- unique(data2$Measure)

# for (x in 1:length((measure))){
#   MEASURE <- measure[x]
#   subset <- data[which(data$Measure==MEASURE),]
#   
#   # create a graph for the raw data of the task
#   ggsave(paste(MEASURE, ".jpeg", sep=""),plot = pirateplot(formula =Values ~ Stimulus,
#                                                         data = subset, #select data frame
#                                                         main = MEASURE, # change title of graph
#                                                         xlab = "", # change x-axis label
#                                                         ylab = paste(MEASURE), # change y-axis label
#                                                         pal = "southpark", # southpark color palette
#                                                         bean.f.o = .6, # Bean fill
#                                                         point.o = .8, # Points
#                                                         inf.f.o = .7, # Inference fill
#                                                         inf.b.o = .8, # Inference border
#                                                         avg.line.o = 1, # Average line
#                                                         inf.f.col = "white", # Inf fill col
#                                                         inf.b.col = "black", # Inf border col
#                                                         avg.line.col = "black", # avg line col
#                                                         point.cex = .7,
#                                                         cex.names=.65,
#                                                         sortx = "sequential"),
#          width = 55, height = 15, units = "cm", dpi = "retina")
# 
# 
# }

for (x in 1:length((measure_bar))){
  MEASURE <- measure_bar[x]
  subset <- data2[which(data2$Measure==MEASURE),]
  # transform the data so that the bars for the Lower IU are presented first (within each condition type)
  subset <- transform(subset,Stimulus = reorder(Stimulus, col_order))
  
  p<- ggplot(subset,
         aes(x= Stimulus,
         y= Mean,
         fill = Stimulus))+
    
    # add the bars to the graphs
    geom_bar(stat     = "identity",
             color    = "black",
             position = pd)+ 
    
    # add the se to the the bars
    geom_errorbar(aes(ymin  = Mean - SE,
                      ymax  = Mean + SE),
                  width = 0.2,
                  size  = 0.7,
                  position = pd,
                  color = "black") +
    
    # remove labels for the x-axis and for the y-axis
    ylab(MEASURE)+xlab("")
  
  # remove x-axis labels and ticks
  p1<- p + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                  # change font on the y-axis to Arial 12
                  axis.text.y =  element_text(size=12, family='Arial'),
                  # remove title of legend and change font to Arial 12
                  legend.title = element_blank(), legend.text = element_text(size=12, family='Arial'),
                 # Remove panel grid lines
                 panel.grid.major = element_blank(),

                 # Remove panel background
                 panel.background = element_blank(),
                 # Add axis line
                 axis.line = element_line(colour = "black"))
  
  # save the plot and name the file "MeanCueISI"
  ggsave(paste(MEASURE,".jpeg", sep = ""),plot = p1, dpi = "retina")
}


