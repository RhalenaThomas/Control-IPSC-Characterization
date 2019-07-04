##############################Score plots

#graphics.off()    

datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined_sampled.csv.csv"
datapath_test = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined_subsample.csv.csv"


Predictees <- c("Good")
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


df <- read.csv(datapath)
df_test <- read.csv(datapath_test)



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


data <- df[,colSums(is.na(df))<nrow(df)]
data <- data_step[rowSums(is.na(data_step)) == 0,]



data_test <- df_test[,colSums(is.na(df_test))<nrow(df_test)]
data_test <- data_test[rowSums(is.na(data_test)) == 0,]


for (Predictee in Predictees) {
  
  variables = paste(Predictee, " ~ ",paste(features,collapse = "+"))
  
  # Set up repeated k-fold cross-validation
  train.control <- trainControl(method = "cv", number = 10)
  # Train the model
  model <- train(as.formula(variables), 
                      data = data,
                      method = "glm", 
                      family="binomial"
  )
  
  exp(coef(model$finalModel))
  
  plot(varImp(model, scale = FALSE))
  
  
  probsTest <- predict(model, newdata=data_test)
  pred      <- factor(probsTest)
  Confusion <- confusionMatrix(pred, data_test$Good)
  fourfoldplot(Confusion$table, color = c("#CC6666", "#99CC99"))
}

