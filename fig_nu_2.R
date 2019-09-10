data_nu <- read.csv("C:/Users/eddie/Desktop/plot3.csv")


colours = c(
  "#b1457b",
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



library(plyr)
library(gtable)
library("ggplot2")
library("gridExtra")
library(grid)
library(dplyr)


samplemap = c("3448-E8" = "E8",
              "3448-MT" = "mTeSR",
              "3450-E8"= "E8",
              "3450-MT" = "mTeSR",
              "AIW001-02-E8"= "E8",
              "AIW001-02-MT" = "mTeSR",
              "AIW002-02-E8"= "E8",
              "AIW002-02-MT" = "mTeSR",
              "AJC001-5-E8"= "E8",
              "AJC001-5-MT" = "mTeSR",
              "AJD002-3E8"= "E8",
              "AJD002-3-MT" = "mTeSR",
              "AJG001-C4-E8"= "E8",
              "AJG001-C4-MT" = "mTeSR",
              "KYOU-E8"= "E8",
              "KYOU-MT" = "mTeSR",
              "NCRM1-E8"= "E8",
              "NCRM1-MT" = "mTeSR",
              "TD02-E8"= "E8",
              "TD02-MT" = "mTeSR",
              "TD03-E8"= "E8",
              "TD03-MT" = "mTeSR",
              "TD10-E8"= "E8",
              "TD10-MT" = "mTeSR")

samples = c("3448-E8" = "3448",
            "3448-MT" = "3448",
            "3450-E8"= "3450",
            "3450-MT" = "3450",
            "AIW001-02-E8"= "AIW001-02",
            "AIW001-02-MT" = "AIW001-02",
            "AIW002-02-E8"= "AIW002-02",
            "AIW002-02-MT" = "AIW002-02",
            "AJC001-5-E8"= "AJC001-5",
            "AJC001-5-MT" = "AJC001-5",
            "AJD002-3E8"= "AJD002-3",
            "AJD002-3-MT" = "AJD002-3",
            "AJG001-C4-E8"= "AJG001-C4",
            "AJG001-C4-MT" = "AJG001-C4",
            "KYOU-E8"= "KYOU",
            "KYOU-MT" = "KYOU",
            "NCRM1-E8"= "NCRM1",
            "NCRM1-MT" = "NCRM1",
            "TD02-E8"= "TD02",
            "TD02-MT" = "TD02",
            "TD03-E8"= "TD03",
            "TD03-MT" = "TD03",
            "TD10-E8"= "TD10",
            "TD10-MT" = "TD10")


data_nu$Media <- revalue(data_nu$Sample.Name, samplemap)
data_nu$Sample.Name <- revalue(data_nu$Sample.Name, samples)


data_nu$Sample.Name <- factor(data_nu$Sample.Name, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3", "TD03","TD10",  "TD22"))

data_nu <- data_nu[,colSums(is.na(data_nu))<nrow(data_nu)]
data_nu <- data_nu[rowSums(is.na(data_nu)) == 0,]



setwd("C:/Users/eddie/Desktop/plots2")

for (target in unique(data_nu[,"Target.Name"])){
  
  data_temp <- data_nu[data_nu[,"Target.Name"]==target,]
  
  data_temp <- data_temp %>% group_by(Sample.Name) %>% summarise(Rq.mean = mean(Rq.mean), Rq.SEM = mean(Rq.SEM))
  
  if (max(data_temp$Rq.mean) > 0.1) { 
    
    p <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
      theme_classic() +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
      geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
      scale_fill_manual(values = colours)+
      labs(x = "Target Name", y = paste(target, " Relative Expression"))+
      #     coord_cartesian(ylim = c(0, 0.1)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1))) 
  } else {
    p <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
      theme_classic() +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
      geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
      scale_fill_manual(values = colours)+
      labs(x = "Target Name", y = paste(target, " Relative Expression"))+
      coord_cartesian(ylim = c(0, 0.1)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1))) 
  }
  
  png(paste('plot_', gsub("/", "-", target), '.png', sep = ""), width = 1000, height = 600)
  #svg(paste('plot_', gsub("/", "-", target), '.svg', sep = ""), width = 1000, height = 600)
  print(p)
  
  dev.off()
}

for (target in unique(data_nu[,"Target.Name"])){
  
  data_temp <- data_nu[data_nu[,"Target.Name"]==target,]
  
  if (max(data_temp$Rq.mean) > 0.1) { 
    
    p <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Media)) +
      theme_classic() +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
      geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
      scale_fill_manual(values = colours)+
      labs(x = "Target Name", y = paste(target, " Relative Expression"))+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  } else {
    p <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Media)) +
      theme_classic() +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
      geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
      scale_fill_manual(values = colours)+
      labs(x = "Target Name", y = paste(target, " Relative Expression"))+
      coord_cartesian(ylim = c(0, 0.1)) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
    
  }
  png(paste('media_plot_', gsub("/", "-", target), '.png', sep = ""), width = 1000, height = 600)
  print(p)
  
  dev.off()
}


