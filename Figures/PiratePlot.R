## THIS SCRIPT CREATES SOME GRAPHS FOR THE NPU TASK
# remove all the variables from the global environment
rm(list = ls())

# load some libraries
library("tidyverse")
library("tidyr")
library("intervals")
library("readxl")
library("data.table")
library("ggplot2")
library("extrafont")
library("ggpirate")
library("yarrr")
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


g1<-ggplot(mpg, aes(x = class, y = cty))+
  geom_violin(aes(fill = class), scale = "width", trim =  FALSE, alpha = 0.45,draw_quantiles = c( 0.5), show.legend = FALSE)+
  geom_jitter(aes(alpha = 0.5),width = 0.05,show.legend = FALSE)

g1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text.x =element_text(size=6))


pirateplot(formula = cty ~ class,
           data = mpg,
           theme = 1,
           main = "theme 1",
           xlab = "",
           ylab = "")

