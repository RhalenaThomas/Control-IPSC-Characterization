##############################Score plots

#graphics.off()    

data4weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/4weeks_combined.csv")
data2weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/2weeks_combined.csv")
dataCNPC = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/CNPC_combined.csv")

data4weeks$Time <-rep("4w",nrow(data4weeks))
data2weeks$Time <-rep("2w",nrow(data2weeks))
dataCNPC$Time <-rep("NPC",nrow(dataCNPC))

df <- rbind(rbind(data4weeks, data2weeks), dataCNPC)


var_list <- c("Directory","Lines","Time")   # directory is going to tell use the media and passage
features <- c("Ch1.positive",
              "Ch1.positive_per_nuclei",
              "Ch1.intensity",
              "Dapi.positive",
              "Dapi.intensity",
              "too.big...250._per_nuclei",
              "size.average",
              "too.small...75._per_nuclei"
              
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
library(agricolae)



df$Well <- paste(df$Row, df$Column, df$Directory, df$Time) 

mean2 = function(x) {
  if (is.numeric(x[1]))
    return(mean(x))
  return(x[1])
}

df <- aggregate(df, by = list(df$Well), mean2)



df$Lines <- factor(df$Column)

data <- df[,colSums(is.na(df))<nrow(df)]
data <- data[rowSums(is.na(data)) == 0,]


for (var1 in var_list) {
  for (var2 in var_list){
    if (var1!= var2){
      for (Feature in features) {
        
        
        variables = paste(Feature, " ~ ", var1, " * ", var2)
        
        print(variables)
        
        res.aov <- aov(as.formula(variables), data = data)
        # Summary of the analysis
        print(summary(res.aov))
        write.csv(as.matrix(res.aov), file = "ANOVA", na = "")
        
        p <- ggplot(data, aes_string(x = var1, y = Feature, fill= var2)) +
          geom_boxplot()
        print(p) 
      }     
    }
  }
  
  datatemp <- data[data[,var1]==val,]
  
}



