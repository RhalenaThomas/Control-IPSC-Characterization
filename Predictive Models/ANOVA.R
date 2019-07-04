##############################Score plots

#graphics.off()    

data4weeks = read.csv("/export03/data/12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined.csv")
data2weeks = read.csv("/export03/data/12CellLinesPaper/output_June11/2weeksoutputRT/2weeks_combined.csv")
dataCNPC = read.csv("/export03/data/12CellLinesPaper/output_June11/outputCNPC/CNPC_combined.csv")

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

df <- aggregate(df, by = list(df$Well), mean2)

df$Score <- df[,"Score"] / 8
df$Growth <- df[,"Growth"] / 2.5
df$Viability <- df[,"Viability"] / 2.5
df$Differentiation <- df[,"Differentiation"] / 2.5
df$Attachment <- df[,"Attachment"] / 2.5

df$Good <- factor(df[,"Score"] > 0.55)

df$Lines <- factor(df$Column)

data <- df[,colSums(is.na(df))<nrow(df)]
data <- data[rowSums(is.na(data)) == 0,]


for (Predictee in Predictees) {
  for (Predictee2 in Predictees){
    for (Feature in features) {
      
      
      
      variables = paste(Feature, " ~ ", Predictee, " * ", Predictee2)
      
      print(variables)
      
      res.aov <- aov(as.formula(variables), data = data)
      # Summary of the analysis
      print(summary(res.aov))
      
      tuk<- TukeyHSD(res.aov)
      print(tuk)
      print(plot(tuk))
      
    }    
  }

  
}


