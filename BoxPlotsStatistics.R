##############################Score plots

data4weeks = read.csv("C:/Users/eddie/Desktop/4weeks_combined.csv")
data2weeks = read.csv("C:/Users/eddie/Desktop/2weeks_combined.csv")
dataCNPC = read.csv("C:/Users/eddie/Desktop/CNPC_combined.csv")

markerDirectory = "C:/Users/eddie/Desktop/boxplot_outputs/markers"
nucleiDirectory = "C:/Users/eddie/Desktop/boxplot_outputs/nuclei"
allDirectory = "C:/Users/eddie/Desktop/boxplot_outputs/all_time_course"

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

channelfeatures <- c(".positive_per_Dapi_nuclei")


grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)*1
  lwidth <- sum(legend$width)*1
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  return(combined)
  
}
# load libraries for plots 

library(ggplot2)
library(RColorBrewer)
library(ggExtra)
library(cowplot)
library(plyr)
library(gtable)

# loop to make a 3D graph for each 'independent' 

#colkey = list(labels = data_input$Column, clab="Cell Line")

#####################################################################################

data4weeks$Time <-rep("4w",nrow(data4weeks))
data2weeks$Time <-rep("2w",nrow(data2weeks))
dataCNPC$Time <-rep("NPC",nrow(dataCNPC))

listdfs = list(data4weeks, dataCNPC)

lapply(listdfs, setDT) # change the `data.frame` to `data.table`

times = c("4 Weeks", "NPC")


for (j in seq_along(listdfs)) { # loop over sequence
  
  dataframe <- as.data.frame(listdfs[j])
  
  dataframe$Lines <- as.factor(dataframe$Column)
  
  dataframe$Lines <- revalue(dataframe$Lines, c("1"="AJC001-5", "2"="AJD002-3", "3"="AJG001-C4", "4"= "AIW001-02", "5"="AIW002-02", "6"= "NCRM1", "7"= "KYOU", "8"="TD02", "9"="TD03", "10"="TD10", "11"="#3448", "12"="#3450"))
  dataframe$Lines <- factor(dataframe$Lines, levels = c("AIW001-02", "AIW002-02", "AJC001-5", "AJD002-3", "AJG001-C4","#3448", "#3450" ,"TD02", "TD03", "TD10", "TD22","NCRM1", "KYOU"))
  
  
    
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
    
#    for (k in unique(dataframe[,channel])){
      
      
      
#      data_input <- dataframe[dataframe[, channel] == k,]
    
#      plots <- list()
#      plots2 <- list()
      
#      for (i in channelfeatures){
  
#        i <- paste(channel, i, sep= "")
        
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
        
#        print(paste(dataframe[1, "Time"], " ", k, " ", i))  
      
        #jpeg(paste(sampleName, i, "plots.jpg"), units="in", width = 10, height = 10, res=300 )
          
        #p = ggplot(data_input, aes_string(x= "X.nuclei", y = i, color = "Lines")) +
        #  geom_point() +
        #  theme(legend.position="left") 
        
        #print(ggMarginal(p, type="density", groupColour = TRUE))
          
#        b = ggplot(data_input, aes_string(x="Lines", y=i)) +
#                geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#                ggtitle(paste(dataframe[1, "Time"], " ", k, " ", i)) + 
#                scale_fill_manual(values = colours) +
#                theme(legend.position="none")
#        plots[[i]] <- b
        
#        b2 = ggplot(data_input, aes_string(x="Lines", y=i)) +
#          geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#          ggtitle(paste(dataframe[1, "Time"], " ", k, " ", i)) + 
#          scale_fill_manual(values = colours) +
#          theme(legend.position= "top")
        
#        plots2[[i]] <- b2
        
#      }
      
#      p = plot_grid(plots[[1]],plots[[2]],plots2[[1]],plots2[[2]], labels = "AUTO")
      
#      p2 = plot_grid(plots[[3]],plots[[4]],plots2[[3]],plots2[[4]], labels = "AUTO")
      
#      setwd(markerDirectory)
#      save_plot(paste(sep = "", dataframe[1, "Time"], " ", k, "positive", ".png") , p, base_height = 10, base_width = 20)
#      save_plot(paste(sep = "", dataframe[1, "Time"], " ", k, "intensity", ".png"), p2,  base_height = 10, base_width = 20)
      
#    }
  
  }
  
  for (l in unique(dataframe$SubDirectory)) {
    
    datatemp <- dataframe[dataframe$SubDirectory==l,]

    plots <- list()
    
    plots[[1]] <- ggplot(datatemp, aes_string(x="Lines", y="Dapi.positive")) +
      geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
      scale_fill_manual(values = colours)+
      theme_classic()+
      theme(axis.text.x = element_blank())+
      labs(y = "Dapi nuclei") +
      theme(axis.title.x = element_blank()) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      theme(text = element_text(size=20))+
      theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))
        
    for (channel in c("Ch1", "Ch2", "Ch3")){
      plots[[channel]] <- ggplot(datatemp, aes_string(x="Lines", y=paste(channel, channelfeatures, sep =""))) +
        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
        scale_fill_manual(values = colours) +
        theme_classic()+
        labs(y = paste(datatemp[1,channel], " positive per Dapi nuclei")) +
        theme(axis.text.x = element_blank())+
        theme(axis.title.x = element_blank()) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        theme(text = element_text(size=20))
      }
    
    p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]], nrow = 2, ncol = 2, position = "right")
    
    
    
    setwd(markerDirectory)
    save_plot(paste(sep = "", times[j], "-", l, ".png") , p, base_height = 10, base_width = 20)
    
    
  }

  #for (i in nucleifeatures) {
    
    
    #print(paste("Removed ", outliers, " outliers"))
    
#      dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
      
#      b = ggplot(dataframe, aes_string(x="Lines", y=i)) +
#        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#        ggtitle(paste(dataframe[1, "Time"], " ", i)) + 
#        scale_fill_manual(values = colours) +
#        theme(legend.position="none")
      
#      b2 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#        ggtitle(paste(dataframe[1, "Time"], " ", i, "per_Dapi_nuclei")) + 
#        scale_fill_manual(values = colours) +
#        theme(legend.position="none") 
      
#      b3 = ggplot(dataframe, aes_string(x="Lines", y=i)) +
#       geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#        ggtitle(paste(dataframe[1, "Time"], " ", i)) + 
#        scale_fill_manual(values = colours)  +
#        theme(legend.position= "top")
  
#      b4 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#        geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#        ggtitle(paste(dataframe[1, "Time"], " ", i,  "per_Dapi_nuclei")) + 
#       scale_fill_manual(values = colours) 
      
#      p = plot_grid(b, b2, b3, b4, labels = "AUTO")
      
#      setwd(nucleiDirectory)
#      save_plot(paste(sep = "", dataframe[1, "Time"], " ", i, ".png") , p, base_height = 10, base_width = 20)
    
#   }
  
  setwd(allDirectory)
  
  print(paste("number of less than 5 nuclei removed: ", nones))
  
  i = "Dapi.positive"

  datatemp <- dataframe
  p <- ggplot(datatemp, aes_string(x="Lines", y=paste(i, sep =""))) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
    scale_fill_manual(values = colours) +
    theme_classic()+
    labs(y = "Dapi nuclei") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position= "none",  axis.title.x = element_blank()) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(text = element_text(size=20))
  
  save_plot(paste(sep = "", times[j], "-Dapi-nuclei" , ".png") , p, base_height = 10, base_width = 20)

  i = "Ch1.positive_per_Dapi_nuclei"
  
  dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
  
  datatemp <- dataframe[dataframe[,"Ch1"] == "MAP2",]
  
  p <- ggplot(datatemp, aes_string(x="Lines", y=paste(i, sep =""))) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
    scale_fill_manual(values = colours) +
    theme_classic()+
    labs(y = "MAP2 positive per Dapi nuclei") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position= "none",  axis.title.x = element_blank()) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(text = element_text(size=20))
  
  save_plot(paste(sep = "", times[j], "-", i , ".png") , p, base_height = 10, base_width = 20)
  
  i = "Ch2.positive_per_Dapi_nuclei"
  
  dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
  
  datatemp <- dataframe[dataframe[,"Ch2"] == "nestin",]
  
  p <- ggplot(datatemp, aes_string(x="Lines", y=paste(i, sep =""))) +
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
    scale_fill_manual(values = colours) +
    theme_classic()+
    labs(y = "Nestin positive per Dapi nuclei") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position= "none",  axis.title.x = element_blank()) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme(text = element_text(size=20))
  
  save_plot(paste(sep = "", times[j], "-", i, ".png") , p, base_height = 10, base_width = 20)
  
  
  
  
}

  

#for (i in allfeatures) {
  
#  dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
  
#  b = ggplot(dataframe, aes_string(x="Lines", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none")
  
#  b2 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", i, "per_Dapi_nuclei")) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none")
  
#  b3 = ggplot(dataframe, aes_string(x="Lines", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top")
#  
#  b4 = ggplot(dataframe, aes_string(x="Lines", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", i,  "per_Dapi_nuclei")) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position= "top") 
#  
#  p = plot_grid(b, b2, b3, b4, labels = "AUTO")
  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all", i, ".png") , p, base_height = 10, base_width = 20)
#  
#}

#############facets

#dataframe$Time <- factor(dataframe$Time, levels = c("NPC", "2w", "4w"))

#for (i in allfeatures) {
  
  
#  b = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none") +
#    facet_grid(. ~ Lines)
  
  
#  b3 = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top") +
#    facet_grid(. ~ Lines)

    
#  p = plot_grid(b,b3,labels = "AUTO", ncol = 1, align = 'v')
  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all_over_time", i, ".png") , p, base_height = 10, base_width = 20)
  
#}



#dataframe <- dataframe[dataframe$Ch1 == "MAP2",]

#for (i in c("Ch1.positive")) {
  
#  dataframe[,paste(i, "per_Dapi_nuclei", sep = "_")] <- dataframe[,i] / dataframe[,"X.nuclei"]
  
#  b = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none") +
#    facet_grid(. ~ Lines)
  
  
#  b3 = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top") +
#    facet_grid(. ~ Lines)
#  
  
#  p = plot_grid(b,b3,labels = "AUTO", ncol = 1, align = 'v')
#  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all_over_time", i, ".png") , p, base_height = 10, base_width = 20)
#  

#  b = ggplot(dataframe, aes_string(x="Time", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", paste(i, "per_Dapi_nuclei", sep = "_"))) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none") +
#    facet_grid(. ~ Lines)
#  
  
#  b3 = ggplot(dataframe, aes_string(x="Time", y=paste(i, "per_Dapi_nuclei", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", paste(i, "per_Dapi_nuclei", sep = "_"))) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top") +
#    facet_grid(. ~ Lines)
  
  
#  p = plot_grid(b,b3,labels = "AUTO", ncol = 1, align = 'v')
#  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all_over_time", paste(i, "per_Dapi_nuclei", sep = "_"), ".png") , p, base_height = 10, base_width = 20)
  
#}


#for (i in c("Ch1.intensity")) {
  
#  dataframe[,paste(i, "per_Dapi_intensity", sep = "_")] <- dataframe[,i] / dataframe[,"Dapi.intensity"]
  
#  b = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none") +
#    facet_grid(. ~ Lines)
  
  
#  b3 = ggplot(dataframe, aes_string(x="Time", y=i)) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", i)) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top") +
#    facet_grid(. ~ Lines)
  
  
#  p = plot_grid(b,b3,labels = "AUTO", ncol = 1, align = 'v')
  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all_over_time", i, ".png") , p, base_height = 10, base_width = 20)
  
#  b = ggplot(dataframe, aes_string(x="Time", y=paste(i, "per_Dapi_intensity", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Lines))+
#    ggtitle(paste( " ", paste(i, "per_Dapi_intensity", sep = "_"))) + 
#    scale_fill_manual(values = colours) +
#    theme(legend.position="none") +
#    facet_grid(. ~ Lines)
  
  
#  b3 = ggplot(dataframe, aes_string(x="Time", y=paste(i, "per_Dapi_intensity", sep = "_"))) +
#    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE, aes(fill = Directory))+
#    ggtitle(paste( " ", paste(i, "per_Dapi_intensity", sep = "_"))) + 
#    scale_fill_manual(values = colours)+
#    theme(legend.position= "top") +
#    facet_grid(. ~ Lines)
  
  
#  p = plot_grid(b,b3,labels = "AUTO", ncol = 1, align = 'v')
  
#  setwd(allDirectory)
#  save_plot(paste(sep = "","all_over_time", paste(i, "per_Dapi_intensity", sep = "_"), ".png") , p, base_height = 10, base_width = 20)
  
  
#}



#third version saved graphs
#aligned columns
#remove below 5 nuclei

