##############################Score plots

graphics.off()    

datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined_sampled.csv.csv"
datapath_test = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined_subsample.csv.csv"


Predictees <- c("Dapi.intensity")
features <- c("Score", 
              "Dapi.area", 
              "Dapi.area.fraction.in.nuclei",
              "Ch1.positive",
              "Ch1.intensity",
              "Ch1.area",
              "all.negative",
              "X.nuclei",
              "too.small...75.",
              "too.big...250.",
              "size.average")

# Set seed for reproducibility
set.seed(42)


df <- read.csv(datapath)

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

ggplotRegression <- function (fit, index) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[index], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste(" Slope =",signif(fit$coef[[index]], 5),
                       " P =",signif(summary(fit)$coef[index,4], 5)))
}


df$Score <- df[,"Score"] / 8
df$Growth <- df[,"Growth"] / 2.5
df$Viability <- df[,"Viability"] / 2.5
df$Differentiation <- df[,"Differentiation"] / 2.5
df$Attachment <- df[,"Attachment"] / 2.5

data_step <- df[,colSums(is.na(df))<nrow(df)]
data_step <- data_step[rowSums(is.na(data_step)) == 0,]



for (Predictee in Predictees) {
  
  variables = paste(Predictee, " ~ ",paste(features,collapse = "+"))
  
  # Set up repeated k-fold cross-validation
  train.control <- trainControl(method = "cv", number = 10)
  # Train the model
  step.model <- train(as.formula(variables), 
                      data = data_step,
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:12),
                      trControl = train.control
  )
  
  print(step.model$results)
  print(step.model$bestTune)
  print(summary(step.model$finalModel))
  modelfeatures  <- rownames(as.data.frame(coef(step.model$finalModel, step.model$bestTune$nvmax)))[-1]
  lmodel <- lm(as.formula(paste(Predictee, " ~ ",paste(modelfeatures,collapse = "+"))), data_step)
  
  stepPlot <- ggplot(step.model)
  
  
  
  relImportance <- calc.relimp(lmodel, type = "lmg", rela = F)  
  cat('Relative Importances: \n')
  sort(round(relImportance$lmg, 3), decreasing=TRUE)
  
  bootsub <- boot.relimp(as.formula(paste(Predictee, " ~ ",paste(modelfeatures,collapse = "+"))), data_step,
                         b = 100, type = 'lmg', rank = TRUE, diff = TRUE, sort = TRUE)
  
  
  
  
  
  
  for (i in 1:step.model$bestTune$nvmax+1){
    #print(ggplotRegression(lmodel, i))  
  }
  
  
  par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
  plot(lmodel)
  par(mfrow=c(1,1)) # Change back to 1 x 1
  
  
  pred <- predict(lmodel, data_step)
  
  print(summary (lmodel))
  print(AIC (lmodel))
  
  actuals_preds <- data.frame(cbind(actuals=data_step[[Predictee]], predicteds=pred))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds)  
  
  minmax <-  mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  #min_max accuracy
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  #mean absolute percent deviation
  
  #test data
  
  df_test <- read.csv(datapath_test)
  
  df_test$Score <- df_test[,"Score"] / 8
  df_test$Growth <- df_test[,"Growth"] / 2.5
  df_test$Viability <- df_test[,"Viability"] / 2.5
  df_test$Differentiation <- df_test[,"Differentiation"] / 2.5
  df_test$Attachment <- df_test[,"Attachment"] / 2.5
  
  data_step_test <- df_test[,colSums(is.na(df))<nrow(df_test)]
  data_step_test <- data_step_test[rowSums(is.na(data_step)) == 0,]
  
  pred_test <- predict(lmodel, data_step_test)
  
  print(summary (lmodel))
  print(AIC (lmodel))
  
  
  
  actuals_preds_test <- data.frame(cbind(actuals=data_step_test[[Predictee]], predicteds=pred_test))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds_test)  
  
  minmax_test <-  mean(apply(actuals_preds_test, 1, min) / apply(actuals_preds_test, 1, max))  #min_max accuracy
  mape_test <- mean(abs((actuals_preds_test$predicteds - actuals_preds_test$actuals))/actuals_preds_test$actuals)  #mean absolute percent deviation
  
  errors <- postResample(pred = pred_test, obs = actuals_preds_test$actuals)
  
  groups <- as.factor(actuals_preds_test$actuals)
  
  p <- ggplot(actuals_preds_test, aes_string(x = "actuals", y = "predicteds")) +
    geom_point() +
    geom_smooth(method = lm) +
    labs(title = paste("2 Weeks Cell Line", Predictee, "prediction")) + 
    geom_line(data = data.frame(x = c(0,1), y = c(0,1)),
              aes(x = x, y = y), colour = "red")
  
  errors["MinMax"] <- minmax_test
  errors["MAPE"] <- mape_test
  
  errors
  
  tab <- as.data.frame(errors)
  
  p_tab <- tableGrob(unname(tab))
  print(p)
  print(ggdraw() + draw_plot(p_tab))
  print(stepPlot)
  print(plot(booteval.relimp(bootsub, level=.95)))
  print(ggcoef(lmodel, exclude_intercept = TRUE, errorbar_height = .2, color = "blue", sort = "ascending") + 
          scale_x_continuous())
  
  Confusion <- confusionMatrix(factor(actuals_preds_test$predicteds > 0.55),factor(actuals_preds_test$actuals > 0.55))
  print(fourfoldplot(Confusion$table, color = c("#CC6666", "#99CC99")))
  
}






