sampleName = "mTeSR1-2 A+B"
datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/output_Cortical-final-4w-11-20-18-mTeSR1-2_Cortical-final-4w-11-20-18-mTeSR1-2-A+B.csv"
# set your directory
saveDataDirectory = "/export03/data/12CellLinesPaper/summaryPlots/NPC/G+H_SOX2_PAX6"
setwd(saveDataDirectory)


df <- read.csv(datapath)
colnames(df)

#filters the data basd on if its blurry (False == not blurry)

df_filtered <- df[df$image.quality == "False",]

# load libraries for plots 

library(plot3D)
library(ggplot2)
library(RColorBrewer)
# loop to make a 3D graph for each 'independent' 

#colkey = list(labels = data_input$Column, clab="Cell Line")

#####################################################################################
df_filtered$Lines <- as.character(df_filtered$Column)
data_input <- df_filtered


data_input = subset(data_input, select = -c(image.quality) )

n <- colnames(data_input[7:34])

for (i in n){
  
  outliers = 0
  
  m = mean(as.numeric(data_input[,i]))
  s = sd(as.numeric(data_input[,i]))
    for (x in 1:length(data_input[,i])){
      if (as.numeric(data_input[x,i]) > m + (2*s)){
          data_input[x,i] = NaN
          outliers = outliers + 1
      }          
    }
  
  print(paste(i, " number of outliers: ", outliers))
  
  #jpeg(paste(sampleName, i, "plots.jpg"), units="in", width = 10, height = 10, res=300 )
  #par(mfrow=c(2,2), mar = c(1,1,1,1))
  
  p = ggplot(data_input, aes_string(x= "Dapi.intensity", y = i, color = "Lines")) +
    geom_point() +
    theme(legend.position="left") 
    
  print(ggMarginal(p, type="density", groupColour = TRUE))

  
  #ggplot(data=data_input, aes_string(x = "X.nuclei", y = i))
  
  #scatter2D(data_input$X.nuclei,as.numeric(data_input[,i]),colvar= data_input$Column, ylab=i, xlab="Nuclei", type = "p")  
  
  #    x_c <- cut(as.numeric(data_input[,i]), 12)
  #    y_c <- cut(data_input$Column, 12)
    
  #    z <- table(x_c, y_c)
    
   #hist3D(z=z, border="black", xlab= i, ylab= "cell line",col = ramp.col(c("yellow","green","blue")))
    
    #image2D(z=z, border="black", ylab= "cell line", xlab= i,col = ramp.col(c("yellow","green","blue")))

    
}


### try to make the column vector a character
library("dplyr")

library(dplyr)
library(caret)
library(tidyverse)

df_filtered$Lines <- as.character(df_filtered$Column)

library(ggExtra )
library(viridis)
p=ggplot(df_filtered, aes(x=X.nuclei, y=nestin.positive, color=Lines, size=nestin.positive)) +
  geom_point() +
  theme(legend.position="left")
ggMarginal(p, type="histogram")
ggMarginal(p, type="density")


geom_smooth(aes(color = Species, fill = Species), method = "lm") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

scale_color_manual(values = c("#FFDB6D", "#C4961A", "#F4EDCA", 
                              "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"))

myPal <- c("#FFDB6D", "#C4961A", "#F4EDCA", "#293352", "#4E84C4",
           "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")

p=ggplot(df_filtered, aes(x=X.nuclei, y=nestin.positive, color=Column)) +
  geom_point(size=5, aes(color = Column)) +
  theme(legend.position="left") +
 
ggMarginal(p, type="histogram")
p + scale_color_manual(values = myPal)


ggMarginal(p, type="density")



ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
  geom_point() +
  theme(legend.position="none")


geom_point(size=6, alpha=0.6)

scale_color_manual(values = c("#FFDB6D", "#C4961A", "#F4EDCA", 
                                              "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"))




# library
library(ggridges)
library(ggplot2)

# Data
head(diamonds)

# basic example
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


scCells = 166242 
set1and2 = 78379 + 87863 
cellperOrg = scCells/21
cellperOrg


#Paula's organoid comparison

      orgANOVA = read.csv("/export02/data/OrgSizeMacro/outputPvA.csv")
      
      
      ggplot(orgANOVA, aes(y = Feret, x= measure, color= File.Name)) +  
        geom_point(size = 5, alpha = 0.5) + 
        geom_line(aes(group = File.Name)) +
        labs(x = "Measurement", y = "Diameter") + 
        theme(text = element_text(size = 18), axis.text = element_text(size = 16), aspect.ratio = 0.80) + ylim(590,825)
      
      
      
      
      orgALL = read.csv("/export02/data/OrgSizeMacro/outputAllDiameter.csv")
      
      
      ggplot(orgALL, aes(y = Diameter, x= Measure)) +  
        geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
        geom_line(aes(group = File.Name)) +
        labs(x = "Measurement", y = "Diameter") + 
        theme(text = element_text(size = 18), axis.text = element_text(size = 16), aspect.ratio = 0.80)
      


#Conrad comparison

    conrad = read.csv("/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/ConradCompare/Image.csv")
    us = read.csv("/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/4weeksoutputRT/output_Cortical-final-4w-11-20-18-mTeSR1-2_Cortical-final-4w-11-20-18-mTeSR1-2-A+B.csv")
    
    #Nuclei
    
    par(mfrow=c(2,2))
    
    hist(conrad$Count_Nuclei, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2500), ylim = c(0, 100), breaks = 40)  # first histogram
    par(new = TRUE)
    hist(us$X.nuclei, col=rgb(1,0,0,1/4), main="Number of nuclei, Conrad vs Us",xlab="Number of nuclei", xlim = c(0, 2500), ylim = c(0, 100), breaks = 20)  # second
    
    #Brn2 Positive
    
    hist(conrad$Count_Brn2, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2500), ylim = c(0, 250), breaks = 20)  # first histogram
    par(new = TRUE)
    hist(us$Brn2.positive, col=rgb(1,0,0,1/4), main="Brn2 positive, Conrad vs Us",xlab="Brn2 positive", xlim = c(0, 2500), ylim = c(0, 250), breaks = 10)  # second
    
    #Proportion
    
    
    hist(conrad$Count_Brn2/conrad$Count_Nuclei, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2), ylim = c(0, 60), breaks = 5000)  # first histogram
    par(new = TRUE)
    hist(us$Brn2.positive/us$X.nuclei, col=rgb(1,0,0,1/4), main="Brn2 positive / nuclei, Conrad vs Us",xlab="Brn2 positive / nuclei", xlim = c(0, 2), ylim = c(0, 60), breaks = 10)  # second
    
    #Scatter
    
    scatter2D(conrad$Count_Nuclei,conrad$Count_Brn2,col=rgb(0,0,1,1/2), ylab="", xlab="", type = "p", xlim = c(0, 3000), ylim = c(0, 2000))  
    par(new = TRUE)
    scatter2D(us$X.nuclei,us$Brn2.positive,col=rgb(1,0,0,1/2), main="Brn2 positive / nuclei, Conrad vs Us", ylab="Brn2 positive", xlab="nuclei", type = "p", xlim = c(0, 3000), ylim = c(0, 2000))  

    
#MAP2
    
    #Nuclei
    
    par(mfrow=c(2,2))
    
    hist(conrad$Count_Nuclei, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2500), ylim = c(0, 100), breaks = 40)  # first histogram
    par(new = TRUE)
    hist(us$X.nuclei, col=rgb(1,0,0,1/4), main="Number of nuclei",xlab="Number of nuclei", xlim = c(0, 2500), ylim = c(0, 100), breaks = 20)  # second
    
    #Brn2 Positive
    
    hist(conrad$Count_Cells, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2500), ylim = c(0, 200), breaks = 20)  # first histogram
    par(new = TRUE)
    hist(us$MAP2.positive, col=rgb(1,0,0,1/4), main="MAP2 positive",xlab="MAP2 positive", xlim = c(0, 2500), ylim = c(0, 200), breaks = 10)  # second
    
    #Proportion
    
    
    hist(conrad$Count_Cells/conrad$Count_Nuclei, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 2), ylim = c(0, 150), breaks = 10)  # first histogram
    par(new = TRUE)
    hist(us$MAP2.positive/us$X.nuclei, col=rgb(1,0,0,1/4), main="MAP2 positive / nuclei",xlab="MAP2 positive / nuclei", xlim = c(0, 2), ylim = c(0, 150), breaks = 10)  # second
    
    #Scatter
    
    scatter2D(conrad$Count_Nuclei,conrad$Count_Cells,col=rgb(0,0,1,1/2), ylab="", xlab="", type = "p", xlim = c(0, 3000), ylim = c(0, 2000))  
    par(new = TRUE)
    scatter2D(us$X.nuclei,us$MAP2.positive,col=rgb(1,0,0,1/2), main="MAP2 positive / nuclei", ylab="MAP2 positive", xlab="nuclei", type = "p", xlim = c(0, 3000), ylim = c(0, 2000))  
    
    
#Size
    
    par(mfrow=c(2,1))
    
    #Brn2 Positive
    
    hist(conrad$Mean_Nuclei_AreaShape_Area, col=rgb(0,0,1,1/4), main="",xlab="",ylab="", xlim = c(0, 1000), ylim = c(0, 250), breaks = 80)  # first histogram
    par(new = TRUE)
    hist(us$size.average, col=rgb(1,0,0,1/4), main="size",xlab="size", xlim = c(0, 1000), ylim = c(0, 250), breaks =10)  # second
    
    #Proportion
    
    scatter2D(conrad$Count_Nuclei,conrad$Mean_Nuclei_AreaShape_Area,col=rgb(0,0,1,1/2), ylab="", xlab="", type = "p", xlim = c(0, 3000), ylim = c(0, 1000))  
    par(new = TRUE)
    scatter2D(us$X.nuclei,us$size.average,col=rgb(1,0,0,1/2), main="size / nuclei", ylab="size", xlab="nuclei", type = "p", xlim = c(0, 3000), ylim = c(0, 1000))  
    
    
##############################Score plots
    
    
    datapath = "/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/output_June11/2weeksoutputRT/2weeks_combined_sampled.csv.csv"
    
    df <- read.csv(datapath)
    colnames(df)
    df_filtered <- df[df$image.quality == "False",]
    
    # load libraries for plots 
    
    library(plot3D)
    library(ggplot2)
    library(RColorBrewer)
    # loop to make a 3D graph for each 'independent' 
    
    #colkey = list(labels = data_input$Column, clab="Cell Line")
    
    #####################################################################################
    df_filtered$Lines <- as.character(df_filtered$Column)
    data_input <- df_filtered
    
    
    data_input = subset(data_input, select = -c(image.quality) )
    
    n <- colnames(data_input[12:40])
    
    
    
    for (i in n){
      
      outliers = 0
      
      m = mean(as.numeric(data_input[,i]), na.rm = TRUE)
      s = sd(as.numeric(data_input[,i]), na.rm = TRUE)
      for (x in 1:length(data_input[,i])){
        if (as.numeric(data_input[x,i]) > m + (2*s)){
          data_input[x,i] = NaN
          outliers = outliers + 1
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
    
    
    
    # Set seed for reproducibility
    set.seed(42)
    # Set up repeated k-fold cross-validation
    train.control <- trainControl(method = "cv", number = 10)
    # Train the model
    step.model <- train(Fertility ~., data = data_input,
                        method = "leapSeq", 
                        tuneGrid = data.frame(nvmax = 1:10),
                        trControl = train.control
    )
    step.model$results
    
    
    
    