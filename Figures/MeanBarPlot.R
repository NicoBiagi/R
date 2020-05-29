### THIS SCRIPT CREATES 2 GRAPHS USING GGPLOT2 ON A MAC 

# remove all the variables from the global environment
rm(list = ls())

# load the ggplot library
library(ggplot2)

# select the colors for the bars (Lower IU = darkturquoise, Higher IU = tomato)
Color = c("darkturquoise", "tomato")

# select the distance between the bars within each condition
pd = position_dodge(1)

# create the dataset for the mean of IU
n<-4

# create the two groups (i.e, Low and High IU)
group1 <- rep(1,n)
group2<- rep(2,n)

# merge the two groups in one variables
group <- c(group1, group2)

# create a variable with the mean for the Lower IU
Lower.IU<- c(-0.057, 0.034, 0.019, 0.004)

# create a variable with the mean for the Higher IU
Higher.IU <- c(-0.035,0.495, -0.376, -0.084)

# merge the two variables together
IU <- c(Lower.IU,Higher.IU)

# create a variable for the se of the Lower IU
se.High <- c(0.058864743, 0.06458087,0.059292723,0.058364759)

# create a variable for the se of the Higher IU
se.Low <- c(0.058864743,0.06458087,0.059292723,0.058364759)

# merge the two variables toghether
se <- c(se.High, se.Low)

# create a variable with the different conditions names
cond <- c("Known No Shock", "Known Predictable Shock", "Known Unpredictable Shock", "Unknown Threat", "Known No Shock", "Known Predictable Shock","Known Unpredictable Shock","Unknown Threat")

# create a dataset including the condition names, group, mean IU and se IU
data<- data.frame(cond,group, IU,se)

# changing the name of the group from number to "Lower IU" and "Higher IU"
data$group[data$group==1]<-"Lower IU"
data$group[data$group==2]<- "Higher IU"

# add a coulmn to the dataset with the order of the bar for the graph
data$col.order = c(1,1,1,1,2,2,2,2)

# transform the data so that the bars for the Lower IU are presented first (within each condition type)
data <- transform(data,group = reorder(group, col.order))

# plot the barplot for the mean of Lower IU and Higher IU
p1<-ggplot(data,                ### The data frame to use.
      aes(x     = cond,
          y     = IU, 
          fill= group) )+
  
  # add the bars to the graphs
  geom_bar(stat     = "identity",
           color    = "black",
           position = pd)+ 
  
  # add the se to the the bars
  geom_errorbar(aes(ymin  = IU - se,
                    ymax  = IU + se),
                width = 0.2,
                size  = 0.7,
                position = pd,
                color = "black") +
  
  # remove labels for the x-axis and for the y-axis
  ylab("")+xlab("")+
  
  # change the colors of the bars
  scale_fill_manual(values = Color) 

# remove x-axis labels and ticks
p2<- p1 + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        # change font on the y-axis to Arial 12
           axis.text.y =  element_text(size=12, family='Arial'),
        # remove title of legend and change font to Arial 12
           legend.title = element_blank(), legend.text = element_text(size=12, family='Arial'))

# save the plot and name the file "MeanBarPlot"
ggsave("MeanBarPlot.jpeg",plot = p2, dpi = "retina")

# create the dataset for the mean of Cue&ISI
# create the two groups (1=Lower IU, 2=Higher IU)
GROUP <- c(1,1,2,2)

# create a variable for the mean Cue & ISI for the Lower IU
mean.Low <- c(0.124,-0.126)

# create a variable for the mean Cue & ISI for the Higher IU
mean.High <- c(0.067, -0.07)

# combine the two variables
MEAN <- c(mean.Low, mean.High)

# create the se for Cue & ISI for the Lower IU
Low.se <- c(0.022, 0.021)

# create the se for Cue & ISI for the Higher IU
High.se <- c(0.022,0.021)

# merge the two se variables together
SE <- c(Low.se, High.se)

# create a variable with the different conditions names
COND <- c("CUE", "ISI", "CUE", "ISI")

# create a dataset composed of conditions, group, mean and se
data2<- data.frame(COND, GROUP, MEAN, SE)

# changing the name of the group from number to "Lower IU" and "Higher IU"
data2$GROUP[data2$GROUP==1]<- "Lower IU"
data2$GROUP[data2$GROUP==2]<- "Higher IU"

# add a coulmn to the dataset with the order of the bar for the graph
data2$col.order2 = c(1,1,2,2)

# transform the data so that the bars for the Lower IU are presented first (within each condition type)
data2 <- transform(data2, GROUP= reorder(GROUP, col.order2))

# plot the barplot for the mean of Lower IU and Higher IU
p3<-ggplot(data2, aes(x=COND, y=MEAN, fill = GROUP))+
  
  # add the bars to the graphs
  geom_bar(stat = "identity", color= "black", position=pd)+
  
  # add the se to the the bars
  geom_errorbar(aes(ymin=MEAN-SE, ymax = MEAN+SE), width = 0.2, size = 0.7, position = pd, color = "black")+
  
  # remove labels for the x-axis and for the y-axis
  ylab("")+xlab("")+
  
  # change the colors of the bars
  scale_fill_manual(values = Color)

# remove x-axis labels and ticks
p4<- p3+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        # change font on the y-axis to Arial 12
        axis.text.y =  element_text(size=12, family='Arial'),
        # remove title of legend and change font to Arial 12
        legend.title = element_blank(), legend.text = element_text(size=12, family='Arial'))

# save the plot and name the file "MeanCueISI"
ggsave("MeanCueISI.jpeg",plot = p4, dpi = "retina")

