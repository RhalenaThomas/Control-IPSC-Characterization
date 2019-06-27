##############################Score plots

graphics.off()   

data4weeks = read.csv("/export03/data/12CellLinesPaper/output_June11/4weeksoutputRT/4weeks_combined.csv")
data2weeks = read.csv("/export03/data/12CellLinesPaper/output_June11/2weeksoutputRT/2weeks_combined.csv")
dataCNPC = read.csv("/export03/data/12CellLinesPaper/output_June11/outputCNPC/CNPC_combined.csv")

markerDirectory = "/export03/data/12CellLinesPaper/boxplots_output/markers"
nucleiDirectory = "/export03/data/12CellLinesPaper/boxplots_output/nuclei"
allDirectory = "/export03/data/12CellLinesPaper/boxplots_output/all_time_course"

colours = c(  "#b1457b",
              "#54b06c",
              "#6d71d8",
              "#c1a339",
              "#5c3788",
              "#799e43",
              "#ca73c6",
              "#43c29e",
              "#b8434e",
              "#36dee6",
              "#b86738",
              "#6a89d5")

nucleifeatures <- c("Dapi.positive", "Dapi.intensity", "size.average", "too.big...250.", "too.small...75.")

allfeatures <- c("Dapi.positive", "size.average", "too.big...250.", "too.small...75.")

channelfeatures <- c(".positive", ".positive_per_Dapi_nuclei", ".intensity", ".intensity_per_Dapi_intensity")



# load libraries for plots 

library(plot3D)
library(ggplot2)
library(RColorBrewer)
library(ggExtra)
library(cowplot)
# loop to make a 3D graph for each 'independent' 

#colkey = list(labels = data_input$Column, clab="Cell Line")

#####################################################################################

data4weeks$Time <-rep("4w",nrow(data4weeks))
data2weeks$Time <-rep("2w",nrow(data2weeks))
dataCNPC$Time <-rep("NPC",nrow(dataCNPC))

listdfs = list(data4weeks, data2weeks, dataCNPC)

lapply(listdfs, setDT) # change the `data.frame` to `data.table`

for (j in seq_along(listdfs)) { # loop over sequence
  
  dataframe <- as.data.frame(listdfs[j])
  
  dataframe$Lines <- as.factor(dataframe$Column)
  
  print("")  
  print(paste("dataframe: ", j))
  
  nones = 0
  
  for (x in 1:length(dataframe[,"X.nuclei"])){
    if (!is.na(dataframe[x,"X.nuclei"])){
      if (as.numeric(dataframe[x,"X.nuclei"]) < 5){
        dataframe[x,"X.nuclei"] = NaN
        nones = nones + 1
      }               
    }
    
  }
  
  print(paste("number of less than 5 nuclei removed: ", nones))
  
  #hist(dataframe$X.nuclei,2000, xlim=c(0, 25))
  
  
  for (channel in c("Ch1", "Ch2", "Ch3")){
    
    dataframe[,paste(channel,".intensity_per_Dapi_intensity", sep = "")] <- dataframe[,paste(channel, ".intensity", sep = "")] / dataframe[,"Dapi.intensity"]
    dataframe[,paste(channel,".positive_per_Dapi_nuclei", sep = "")] <- dataframe[,paste(channel, ".positive", sep = "")] / dataframe[,"Dapi.positive"]
    
    for (k in unique(dataframe[,channel])){
      
      
      
      data_input <- dataframe[dataframe[, channel] == k,]
    
      plots <- list()
      plots2 <- list()
      
      for (i in channelfeatures){
  
        i <- paste(channel, i, sep= "")
        
        #outliers = 0
        #m = mean(as.numeric(data_input[,i]), na.rm = TRUE)
        #s = sd(as.numeric(data_input[,i]), na.rm = TRUE)
        #for (x in 1:length(data_input[,i])){
        #  if (!is.na(data_input[x,i])){
        #    if (as.numeric(data_input[x,i]) > m + (2*s)){
        #      data_input[x,i] = NaN
        #      outliers = outliers + 1
        #    }               
        #  }
        #  
        #}
        
      
        #print(paste(k, " ", i, " number of outliers: ", outliers))
        
        print(paste(dataframe[1, "Time"], " ", k, " ", i))  
      
        #jpeg(paste(sampleName, i, "plots.jpg"), units="in", width = 10, height = 10, res=300 )
          
        #p = ggplot(data_input, aes_string(x= "X.nuclei", y = i, color = "Lines")) +
        #  geom_point() +
        #  theme(legend.position="left") 
        
        #print(ggMarginal(p, type="density", groupColour = TRUE))
          
        b = ggplot(data_input, aes_string(x="Lines", y=i)) +
                geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
                ggtitle(paste(dataframe[1, "Time"], " ", k, " ", i)) + 
                scale_fill_manual(values = colours) +
                theme(legend.position="none")
        plots[[i]] <- b
        
        b2 = ggplot(data_input, aes_string(x="Lines", y=i)) +
          geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
          ggtitle(paste(dataframe[1, "Time"], " ", k, " ", i)) + 
          scale_fill_manual(values = colours) +
          theme(legend.position= "top")
        
        plots2[[i]] <- b2
        
      }
      
      p = plot_grid(plots[[1]],plots[[2]],plots2[[1]],plots2[[2]], labels = "AUTO")
      
      p2 = plot_grid(plots[[3]],plots[[4]],plots2[[3]],plots2[[4]], labels = "AUTO")
      
      setwd(markerDirectory)
      save_plot(paste(sep = "", dataframe[1, "Time"], " ", k, "positive", ".png") , p, base_height = 10, base_width = 20)
      save_plot(paste(sep = "", dataframe[1, "Time"], " ", k, "intensity", ".png"), p2,  base_height = 10, base_width = 20)
      
    }
  }

  for (i in nucleifeatures) {
    
    
    print(paste("Removed ", outliers, " outliers"))
    
      dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
      
      b = ggplot(dataframe, aes_string(x="Lines", y=i)) +
        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
        ggtitle(paste(dataframe[1, "Time"], " ", i)) + 
        scale_fill_manual(values = colours) +
        theme(legend.position="none")
      
      b2 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
        ggtitle(paste(dataframe[1, "Time"], " ", i, "per_Dapi_nuclei")) + 
        scale_fill_manual(values = colours) +
        theme(legend.position="none") 
      
      b3 = ggplot(dataframe, aes_string(x="Lines", y=i)) +
        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
        ggtitle(paste(dataframe[1, "Time"], " ", i)) + 
        scale_fill_manual(values = colours)  +
        theme(legend.position= "top")
  
      b4 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
        ggtitle(paste(dataframe[1, "Time"], " ", i,  "per_Dapi_nuclei")) + 
        scale_fill_manual(values = colours) 
      
      p = plot_grid(b, b2, b3, b4, labels = "AUTO")
      
      setwd(nucleiDirectory)
      save_plot(paste(sep = "", dataframe[1, "Time"], " ", i, ".png") , p, base_height = 10, base_width = 20)
    
    }
  
  
}

dataframe <- rbind(rbind(data4weeks, data2weeks), dataCNPC)
dataframe$Lines <- as.factor(dataframe$Column)


nones = 0

for (x in 1:length(dataframe[,"X.nuclei"])){
  if (!is.na(dataframe[x,"X.nuclei"])){
    if (as.numeric(dataframe[x,"X.nuclei"]) < 5){
      dataframe[x,"X.nuclei"] = NaN
      nones = nones + 1
    }               
  }
  
}

print(paste("number of less than 5 nuclei removed: ", nones))


for (i in allfeatures) {
  
  dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
  
  b = ggplot(dataframe, aes_string(x="Lines", y=i)) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
    ggtitle(paste( " ", i)) + 
    scale_fill_manual(values = colours) +
    theme(legend.position="none")
  
  b2 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
    ggtitle(paste( " ", i, "per_Dapi_nuclei")) + 
    scale_fill_manual(values = colours) +
    theme(legend.position="none")
  
  b3 = ggplot(dataframe, aes_string(x="Lines", y=i)) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
    ggtitle(paste( " ", i)) + 
    scale_fill_manual(values = colours)+
    theme(legend.position= "top")
  
  b4 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
    ggtitle(paste( " ", i,  "per_Dapi_nuclei")) + 
    scale_fill_manual(values = colours) +
    theme(legend.position= "top") 
  
  p = plot_grid(b, b2, b3, b4, labels = "AUTO")
  
  setwd(allDirectory)
  save_plot(paste(sep = "","all", i, ".png") , p, base_height = 10, base_width = 20)
  
}

print("Done")

#third version saved graphs
#aligned columns
#remove below 5 nuclei

