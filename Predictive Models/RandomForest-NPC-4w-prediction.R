##############################Score plots

#graphics.off()    

datapath_4weeks = "C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/4weeks_combined.csv"
datapath_CNPC = "C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/CNPC_combined.csv"

datapath_4weeks_test = "C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/4weeks_combined.csv"
datapath_CNPC_test = "C:/Users/eddie/Documents/Control-IPSC-Characterization/stuff/CNPC_combined.csv"



Predictees <- c("Ch1_4weeks_intensity")
features <- c("Dapi.intensity", 
              "Dapi.area", 
              "Dapi.area.fraction.in.nuclei",
              "Ch1.positive",
              "Ch1.intensity",
              "Ch1.area",
              "all.negative",
              "X.nuclei",
              "too.small...75.",
              "too.big...250.",
              "size.average"
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
library(randomForest) 

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

df_4weeks <- read.csv(datapath_4weeks)
df_CNPC <- read.csv(datapath_CNPC)

df_4weeks_test <- read.csv(datapath_4weeks_test)
df_CNPC_test <- read.csv(datapath_CNPC_test)

df_4weeks$Group <- paste( df_4weeks$Column, df_4weeks$Directory) 
df_CNPC$Group <- paste( df_CNPC$Column, df_CNPC$Directory) 

df_4weeks_test$Group <- paste( df_4weeks_test$Column, df_4weeks_test$Directory) 
df_CNPC_test$Group <- paste( df_CNPC_test$Column, df_CNPC_test$Directory) 

df_grouped <- aggregate(df_4weeks, list(df_4weeks$Group), mean)
df_grouped_test <- aggregate(df_4weeks_test, list(df_4weeks_test$Group), mean)


for (row in 1:nrow(df_CNPC)) {
  df_CNPC[row, "Ch1_4weeks_intensity"] <- df_grouped[which(df_grouped$Group.1 == df_CNPC[row, "Group"]), "Ch1.intensity"] 
}

for (row in 1:nrow(df_CNPC_test)) {
  df_CNPC_test[row, "Ch1_4weeks_intensity"] <- df_grouped_test[which(df_grouped_test$Group.1 == df_CNPC_test[row, "Group"]), "Ch1.intensity"] 
}

df <- df_CNPC
df_test <- df_CNPC_test

df$Score <- df[,"Score"] / 8
df$Growth <- df[,"Growth"] / 2.5
df$Viability <- df[,"Viability"] / 2.5
df$Differentiation <- df[,"Differentiation"] / 2.5
df$Attachment <- df[,"Attachment"] / 2.5

df_test$Score <- df_test[,"Score"] / 8
df_test$Growth <- df_test[,"Growth"] / 2.5
df_test$Viability <- df_test[,"Viability"] / 2.5
df_test$Differentiation <- df_test[,"Differentiation"] / 2.5
df_test$Attachment <- df_test[,"Attachment"] / 2.5

df$Good <- factor(df[,"Score"] > 0.55)
df_test$Good <- factor(df_test[,"Score"] > 0.55)

df$Lines <- factor(df$Column)
df_test$Lines <- factor(df_test$Column)

data <- df[,colSums(is.na(df))<nrow(df)]
data <- data[rowSums(is.na(data_step)) == 0,]



data_test <- df_test[,colSums(is.na(df_test))<nrow(df_test)]
data_test <- data_test[rowSums(is.na(data_test)) == 0,]


for (Predictee in Predictees) {
  
  variables = paste(Predictee, " ~ ",paste(features,collapse = "+"))
  
  # Set up repeated k-fold cross-validation
  train.control <- trainControl(method = "cv", number = 10)
  # Train the model
  model <- randomForest(as.formula(variables),  
                        ntree = 100,
                        data = data)
  print(plot(model))  
  
  exp(coef(model$finalModel))
  imp <- varImp(model, scale = FALSE)
  
  #barplot(names.arg= rownames(imp), height =  imp$Overall)
  
  probsTest <- predict(model, newdata=data_test)
  pred      <- factor(probsTest)
  Confusion <- confusionMatrix(pred, data_test[,Predictee])
  print(Confusion)
  print(ggplotConfusionMatrix(Confusion))
  print(fourfoldplot(Confusion$table, color = c("#CC6666", "#99CC99")))
}

