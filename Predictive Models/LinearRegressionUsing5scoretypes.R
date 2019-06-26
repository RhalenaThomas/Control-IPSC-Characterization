##############################Score plots


datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/2weeksoutputRT/2weeks_combined_sampled.csv.csv"

df <- read.csv(datapath)
colnames(df)

# load libraries for plots 

library(plot3D)
library(ggplot2)
library(RColorBrewer)
# loop to make a 3D graph for each 'independent' 

#colkey = list(labels = data_input$Column, clab="Cell Line")

#####################################################################################
data_input <- df

n <- colnames(data_input[-1:-15])



for (i in n){
  
  outliers = 0
  
  m = mean(as.numeric(data_input[,i]), na.rm = TRUE)
  s = sd(as.numeric(data_input[,i]), na.rm = TRUE)
  for (x in 1:length(data_input[,i])){
    if (!is.na(data_input[x,i])){
      if (as.numeric(data_input[x,i]) > m + (2*s)){
        data_input[x,i] = NaN
        outliers = outliers + 1
      }               
    }
    
  }
  
  print(paste(i, " number of outliers: ", outliers))
  
  #jpeg(paste(sampleName, i, "plots.jpg"), units="in", width = 10, height = 10, res=300 )
  #par(mfrow=c(2,2), mar = c(1,1,1,1))
  
  p = ggplot(data_input, aes_string(x= "Score", y = i, color = "X.nuclei")) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, 6), se = FALSE)   
  
  
  print(p)
  
}


data_step <- df[,colSums(is.na(df))<nrow(df)]
data_step <- data_step[rowSums(is.na(data_step)) == 0,]



library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Set seed for reproducibility
set.seed(42)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Score ~ Dapi.positive + Dapi.intensity+ Dapi.area+ Dapi.area.fraction.in.nuclei+ Ch1.positive+ Ch1.intensity+ Ch1.area+ all.negative+ X.nuclei+ too.small...75. + too.big...250. + size.average+ Viability+ Growth+ Differentiation+ Attachment, 
                    data = data_step,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:12),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)    
coef(step.model$finalModel, 12)

ggplotRegression <- function (fit, index) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[index + 1], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(" Slope =",signif(fit$coef[[index+1]], 5),
                       " P =",signif(summary(fit)$coef[index+1,4], 5)))
}

for (i in 1:15){
  print(ggplotRegression(lm(Score ~ Dapi.intensity+  Dapi.area.fraction.in.nuclei+ Ch1.positive+ Ch1.intensity+ Ch1.area+  X.nuclei + too.small...75. + too.big...250. + size.average+ Viability+ Growth+ Differentiation+ Attachment, 
                            data = data_step), i))  
}

lmodel = lm(Score ~ Dapi.intensity+  Dapi.area.fraction.in.nuclei+ Ch1.positive+ Ch1.intensity+ Ch1.area+  X.nuclei + too.small...75. + too.big...250. + size.average+ Viability+ Growth+ Differentiation+ Attachment, 
            data = data_step)

par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(lmodel)

par(mfrow=c(1,1)) # Change back to 1 x 1


pred <- predict(lmodel, data_step)

summary (lmodel)
AIC (lmodel)

actuals_preds <- data.frame(cbind(actuals=data_step$Score, predicteds=pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

minmax <-  mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  #min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  #mean absolute percent deviation

ggplot(actuals_preds, aes_string(x= "actuals", y = "predicteds")) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = paste("Min-Max Accuracy: ",minmax, "       " ,
                     "Mean Absolute Percentage Error: ",mape))

######################test data


datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/2weeksoutputRT/2weeks_combined_subsample_sampled.csv.csv_sampled.csv.csv"

df <- read.csv(datapath)
colnames(df)  

data_step <- df[,colSums(is.na(df))<nrow(df)]
data_step <- data_step[rowSums(is.na(data_step)) == 0,]

pred <- predict(lmodel, data_step)

summary (lmodel)
AIC (lmodel)

actuals_preds <- data.frame(cbind(actuals=data_step$Score, predicteds=pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

minmax <-  mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  #min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  #mean absolute percent deviation

ggplot(actuals_preds, aes_string(x= "actuals", y = "predicteds")) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = paste("Min-Max Accuracy: ",minmax, "       " ,
                     "Mean Absolute Percentage Error: ",mape))    