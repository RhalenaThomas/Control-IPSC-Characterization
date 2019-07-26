##############################Score plots

#graphics.off()    

data4weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/4weeks_combined.csv")
data2weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/2weeks_combined.csv")
dataCNPC = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/CNPC_combined.csv")

data4weeks$Time <-rep("4w",nrow(data4weeks))
data2weeks$Time <-rep("2w",nrow(data2weeks))
dataCNPC$Time <-rep("NPC",nrow(dataCNPC))

df <- rbind(rbind(data4weeks, data2weeks), dataCNPC)


Predictees <- c("Directory")
features <- c("Ch1.positive",
              "Ch1.positive_per_nuclei",
              "Ch1.intensity",
              "Dapi.positive",
              "Dapi.intensity"

)

# Set seed for reproducibility
set.seed(42)


# load libraries
library(cowplot)
library(gridExtra)  
library(ggplot2)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(relaimpo)
library(GGally)
library(scales)




df$Well <- paste(df$Row, df$Column, df$Directory, df$Time) 

mean2 = function(x) {
  if (is.numeric(x[1]))
    return(mean(x))
  return(x[1])
}

data <- aggregate(df, by = list(df$Well), mean2)

for (time in c("NPC", "2w", "4w")) {
 
  dft <- df[df$Time == time,]
  datat <- data[data$Time == time,]
  for (group in datat$Group.1) {
    temp <- dft[dft$Well == group,]
    row = with(temp, which.min(abs(Dapi.positive - median(temp$Dapi.positive))))
    print(paste(temp[row, "Directory"],temp[row, "SubDirectory"],temp[row, "Image"], sep = "/"))
  }
}


