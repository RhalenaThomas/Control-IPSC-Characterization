library(plyr)
library(gtable)
library("ggplot2")
library("gridExtra")
library(grid)
library(dplyr) 

# Function for shared legend

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




                          ###############
###########################   Fig 1 C   ###############################
                          ###############


data <- read.csv("C:/Users/eddie/Desktop/Absorbtion.csv")
data$Time <- as.factor(data$Time)
data$Sample <- factor(data$Sample, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3","TD03","TD10", "TD22"))
data$Media <- factor(data$Media, levels = c("mTeSR","E8"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

p <- ggplot(data, aes(x=Media, y=Abs, fill=Time)) +
  geom_errorbar(aes(ymin=Abs-SE, ymax=Abs+SE), width=0.75, position=position_dodge(), size = 1) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75, color = 'black', size = 0.5) +
  theme_classic()+
  labs(y = "Absorbtion (D540)") +
  #scale_fill_manual(values = c("#777777", "#eeeeee")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))+
  facet_grid(~Sample, drop = TRUE, scales ="free_x")

print(p)




                          ###############
###########################   Fig 2 B   ###############################
                          ###############

colours = c("#bbbbbb",
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

data <- read.csv("C:/Users/eddie/Desktop/sum-control-paper.csv")
data$name <- factor(data$name, levels = c("H9", "NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3","TD03","TD10", "TD22"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]


plots2 <- list()

for (val in c("OCT/DAPI", "SSEA4/DAPI")) {
  datatemp <- data[data[,"col"]==val,]
  
  plots2[[val]] <- ggplot(datatemp, aes(x=name, y=mean, fill=name)) +
    geom_bar(stat="identity", color = 'black') +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.75) +
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste(val)) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(0.7)), legend.key.size= unit(0.7, "cm"))
  #print(plots[[val]])
}


p <- grid_arrange_shared_legend(plots2[[1]],plots2[[2]], position = "right")

print(grid.draw(p))



                          ###############
###########################   Fig 2 C   ###############################
                          ###############


colours = c("#bbbbbb",
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

data <- read.csv("C:/Users/eddie/Desktop/pluripotency2.csv")

data$Sample.Name <- factor(data$Sample.Name, levels = c("H9", "NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD03", "3448", "3450", "AJD002-3","TD10",  "TD22"))
data$Media <- revalue(data$Sample.Name, c("H9"="mTeSR", "NCRM1"="mTeSR", "KYOU"="mTeSR", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C5"="mTeSR", "TD02"="mTeSR", "TD03" = "E8", "TD10"="E8"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

plots <- list()

for (val in c("OCT3-4", "NANOG", "SOX2", "C-MYC", "KLF4", "ZFP42")) {
  datatemp <- data[data[,"Target.Name"]==val,]
  
  plots[[val]] <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
    geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge(), size = 1) +
    geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste(val, " Relative Expression")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x")
  #print(plots[[val]])
}

p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]], plots[[5]], plots[[6]], nrow = 2, ncol = 3, position = "right")

print(grid.draw(p))





                          ###############
###########################   Fig 3 B   ###############################
                          ###############


data <- read.csv("C:/Users/eddie/Desktop/stability.csv")

colours1 = c("#bbbbbb",
             "#b1457b",
             "#54b06c",
             "#6d71d8",
             "#c1a339",
             "#5c3788",
             "#43c29e")

colours2 = c("#bbbbbb",
             "#799e43",
             "#ca73c6",
             "#b8434e",
             "#36dee6",
             "#b86738",
             "#6a89d5")

data$Sample.Name <- factor(data$Sample.Name, levels =c("Control", "NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "#3448", "#3450", "AJD002-3", "TD03","TD10",  "TD22"))
data$Target.Name <- factor(data$Target.Name, levels =c("chr1", "chr8", "chr10", "chr12", "chr17", "chr18", "chr20", "chrX"))

unique(data$Sample.Name)

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

data_group1 <- data
data_group1$Sample.Name <-  factor(data_group1$Sample.Name, levels =c("Control", "NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02"))

data_group1 <- data_group1[,colSums(is.na(data_group1))<nrow(data_group1)]
data_group1 <- data_group1[rowSums(is.na(data_group1)) == 0,]

data_group2 <- data
data_group2$Sample.Name <-  factor(data_group2$Sample.Name, levels =c("Control", "#3448", "#3450", "AJD002-3","TD03","TD10","TD22"))

data_group2 <- data_group2[,colSums(is.na(data_group2))<nrow(data_group2)]
data_group2 <- data_group2[rowSums(is.na(data_group2)) == 0,]

p1 <- ggplot(data_group1, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black', size = 1) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75), size = 1) +
  scale_fill_manual(values = colours1)+
  labs(x = "Chromosome region", y = "Relative Quantification")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ 
  geom_hline(yintercept=2, linetype="dashed", color = "black")

p2 <- ggplot(data_group2, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black', size = 1) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75), size = 1) +
  scale_fill_manual(values = colours2)+
  labs(x = "Chromosome region", y = "Relative Quantification")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=2, linetype="dashed", color = "black")

print(grid.arrange(p1, p2, ncol = 1))




                          ###############
###########################   Fig 4 B   ###############################
                          ###############




data <- read.csv("C:/Users/eddie/Desktop/trilineage_new.csv")

colours = c(
  "#bbbbbb",
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

data$Sample.Name <- factor(data$Sample.Name, levels = c("H9", "NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "#3448", "#3450", "AJD002-3", "TD03","TD10",  "TD22"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

plots <- list()

for (val in c("PAX6", "NCAM", "MIXL1", "VIM")) {
  data_temp <- data[data[,"Target.Name"]==val,]
  data_temp <- data_temp %>% group_by(Sample.Name) %>% summarise(Rq.mean = mean(Rq.mean), Rq.SD = mean(Rq.SD))
  
  data_temp$Media <- revalue(data_temp$Sample.Name, c("H9"="mTeSR", "NCRM1"="mTeSR", "KYOU"="mTeSR", "#3448"="E8", "#3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C4"="mTeSR", "TD02"="mTeSR", "TD03" = "E8", "TD10"="E8"))
  
  
  plots[[val]] <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
    geom_errorbar(aes(ymin=Rq.mean-Rq.SD, ymax=Rq.mean+Rq.SD), width=0.5, position=position_dodge(), size = 1) +
    geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste("Relative Expression"), title = val) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  #print(plots[[val]])
}


p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]], nrow = 2, ncol = 2, position = "right")

print(grid.draw(p))

                          ###############
###########################   Fig 5 D   ###############################
                          ###############


data <- read.csv("C:/Users/eddie/Desktop/data_NPC_final.csv")

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


samples = c("3448-E8" = "3448",
            "3448-MT" = "3448",
            "3450-E8"= "3450",
            "3450-MT" = "3450",
            "AIW001-02-e8"= "AIW001-02",
            "AIW001-02-MT" = "AIW001-02",
            "AIW002-02-e8"= "AIW002-02",
            "AIW002-02-MT" = "AIW002-02",
            "AJC001-5-E8"= "AJC001-5",
            "AJC001-5-MT" = "AJC001-5",
            "AJD002-3-E8"= "AJD002-3",
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


data$Sample.Name <- revalue(data$Sample.Name, samples)
data$Sample.Name <- factor(data$Sample.Name, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3", "TD03","TD10",  "TD22"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

plots <- list()

for (val in c("NES", "NCAM", "SLC1A3", "ASCL1", "PAX6", "MAP2", "TUB3")) {
  data_temp <- data[data[,"Target.Name"]==val,]
  data_temp <- data_temp %>% group_by(Sample.Name) %>% summarise(Rq.mean = mean(Rq.mean), Rq.SD = mean(Rq.SD))
  
  data_temp$Media <- revalue(data_temp$Sample.Name, c("NCRM1"="mTeSR", "KYOU"="mTeSR", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C4"="mTeSR", "TD02"="mTeSR", "TD03" = "E8", "TD10"="E8"))
  
  
  plots[[val]] <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
    geom_errorbar(aes(ymin=Rq.mean-Rq.SD, ymax=Rq.mean+Rq.SD), width=0.5, position=position_dodge(), size = 1) +
    geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste("Relative Expression"), title = val) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    #geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  #print(plots[[val]])
}


p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],  nrow = 3, ncol = 3, position = "right")

print(grid.draw(p))

plots2 <-list()

for (val in c("SOX1", "SOX9", "NANOG", "OCT3/4", "MIXL1", "AFP")) {
  data_temp <- data[data[,"Target.Name"]==val,]
  data_temp <- data_temp %>% group_by(Sample.Name) %>% summarise(Rq.mean = mean(Rq.mean), Rq.SD = mean(Rq.SD))
  
  data_temp$Media <- revalue(data_temp$Sample.Name, c("NCRM1"="mTeSR", "KYOU"="mTeSR", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C4"="mTeSR", "TD02"="mTeSR", "TD03" = "E8", "TD10"="E8"))
  
  
  plots2[[val]] <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
    geom_errorbar(aes(ymin=Rq.mean-Rq.SD, ymax=Rq.mean+Rq.SD), width=0.5, position=position_dodge(), size = 1) +
    geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste("Relative Expression"), title = val) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    #geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x") +
    coord_cartesian(ylim = c(0, 0.1)) 
  #print(plots[[val]])
}


p <- grid_arrange_shared_legend(plots2[[1]],plots2[[2]],plots2[[3]],plots2[[4]],plots2[[5]],plots2[[6]], nrow = 3, ncol = 2, position = "right")

print(grid.draw(p))



                          ###############
###########################   Fig 6 D   ###############################
                          ###############


data <- read.csv("C:/Users/eddie/Desktop/4W_plot_final.csv")

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

data$Sample.Name <- factor(data$Sample.Name, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3", "TD03","TD10",  "TD22"))

data <- data[,colSums(is.na(data))<nrow(data)]
data <- data[rowSums(is.na(data)) == 0,]

plots <- list()

for (val in c("MAP2", "Tuj","NeuN", "STAB2", "S100B", "NCAM1", "PAX6" )) {
  data_temp <- data[data[,"Target.Name"]==val,]
  data_temp <- data_temp %>% group_by(Sample.Name) %>% summarise(Rq.mean = mean(Rq.mean), Rq.SD = mean(Rq.SD))
  
  data_temp$Media <- revalue(data_temp$Sample.Name, c("NCRM1"="mTeSR", "KYOU"="mTeSR", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C4"="mTeSR", "TD02"="mTeSR", "TD03" = "E8", "TD10"="E8"))
  
  if (max(data_temp$Rq.mean) < 0.1) {
    plots[[val]] <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SD, ymax=Rq.mean+Rq.SD), width=0.5, position=position_dodge(), size = 1) +
      geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
      scale_fill_manual(values = colours)+
      theme_classic()+
      theme(axis.text.x = element_blank())+
      coord_cartesian(ylim = c(0, 0.1)) +
      theme(legend.position = "none", axis.title.x = element_blank()) +
      labs(y = paste("Relative Expression"), title = val) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      #geom_hline(yintercept=1, linetype="dashed", color = "black")+
      theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
      facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
    #print(plots[[val]])   
  } else {
    plots[[val]] <- ggplot(data_temp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
      geom_errorbar(aes(ymin=Rq.mean-Rq.SD, ymax=Rq.mean+Rq.SD), width=0.5, position=position_dodge(), size = 1) +
      geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
      scale_fill_manual(values = colours)+
      theme_classic()+
      theme(axis.text.x = element_blank())+
      theme(legend.position = "none", axis.title.x = element_blank()) +
      labs(y = paste("Relative Expression"), title = val) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      #geom_hline(yintercept=1, linetype="dashed", color = "black")+
      theme(legend.text=element_text(size=rel(0.8)), legend.key.size= unit(0.8, "cm"))+
      facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
    #print(plots[[val]])
  }
}

p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],plots[[7]],  nrow = 3, ncol = 3, position = "right")

print(grid.draw(p))




