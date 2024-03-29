data_tri <- read.csv("C:/Users/eddie/Desktop/Trilineage.csv")
data_stab <- read.csv("C:/Users/eddie/Desktop/stability.csv")
data_plu <- read.csv("C:/Users/eddie/Desktop/pluripotency2.csv")
data_nu <- read.csv("C:/Users/eddie/Desktop/data.csv")


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



#make sure there is rel in data
data_tri$Sample.Name <- factor(data_tri$Sample.Name, levels = c("H9", "NCRM1", "KYOU", "3448", "3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD10"))
data_plu$Sample.Name <- factor(data_plu$Sample.Name, levels = c("H9", "NCRM1", "KYOU", "3448", "3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD10"))
data_stab$Sample.Name <- factor(data_stab$Sample.Name, levels = c("Control", "NCRM1", "KYOU", "#3448", "#3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "TD10"))




data_plu <- data_plu[,colSums(is.na(data_plu))<nrow(data_plu)]
data_plu <- data_plu[rowSums(is.na(data_plu)) == 0,]

data_stab <- data_stab[,colSums(is.na(data_stab))<nrow(data_stab)]
data_stab <- data_stab[rowSums(is.na(data_stab)) == 0,]



library(plyr)
library(gtable)
library("plotly")
library("ggplot2")
library("gridExtra")
library(grid)

#p <- ggplot(data_stab, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
#  theme_classic() +
#  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.6) +
#  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.6, position=position_dodge(width = 0.75)) +
#  scale_fill_manual(values = colours)
#p

data_group1 <- data_stab

data_group1$Sample.Name <-  factor(data_group1$Sample.Name, levels =c("Control","NCRM1", "KYOU", "#3448", "#3450", "AJD002-3", "TD22"))

data_group1 <- data_group1[,colSums(is.na(data_group1))<nrow(data_group1)]
data_group1 <- data_group1[rowSums(is.na(data_group1)) == 0,]

data_group2 <- data_stab

data_group2$Sample.Name <-  factor(data_group2$Sample.Name, levels =c("Control", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "TD10"))

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




p <- ggplot(data_nu, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
  #scale_fill_manual(values = colours)+
  labs(x = "Target Name", y = "Relative Expression")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

p


samples = c("3448",
            "3450",
            "AIW001-02",
            "aiw002-02",
            "AJC001-5",
            "AJD002-3",
            "AJG001-C4",
            "KYOU",
            "NCRM1",
            "TD02",
            "TD03",
            "TD10")

for (sample in samples){

  data_temp <- data_nu
  
  data_temp$Sample.Name <- factor(data_temp$Sample.Name, levels = c(paste(sample, "-E8", sep = ""), paste(sample, "-MT", sep = "")))
  
  data_temp <- data_temp[,colSums(is.na(data_temp))<nrow(data_temp)]
  data_temp <- data_temp[rowSums(is.na(data_temp)) == 0,]
  
  
  p <- ggplot(data_temp, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
    theme_classic() +
    geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
    geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("blue", "orange"))+
    labs(x = "Target Name", y = "Relative Expression")+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_hline(yintercept=1, linetype="dashed", color = "black")
  
  print(p)
  
}

samplesE8 <- paste(samples, "-E8", sep = "")

data_temp <- data_nu

data_temp$Sample.Name <- factor(data_temp$Sample.Name, levels = samplesE8)

data_temp <- data_temp[,colSums(is.na(data_temp))<nrow(data_temp)]
data_temp <- data_temp[rowSums(is.na(data_temp)) == 0,]

p <- ggplot(data_temp, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.75, color = 'black') +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
  scale_fill_manual(values = colours)+
  labs(x = "Target Name", y = "Relative Expression")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

print(p)



plots <- list()

for (val in unique(data_plu[,"Target.Name"])) {
  datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
  plots[[val]] <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
    geom_bar(stat="identity", position=position_dodge(), width = 1, color = 'black', size = 1) +
    geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge(), size = 1) +
    
    scale_fill_manual(values = colours)+
    theme_classic()+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "none", axis.title.x = element_blank()) +
    labs(y = paste(val, " Relative Expression")) +
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    geom_hline(yintercept=1, linetype="dashed", color = "black")+
    theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))
  
}


p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]], plots[[5]], plots[[6]], nrow = 2, ncol = 3, position = "right")

print(grid.draw(p))





#library(plotly)
#plots <- list()

#for (val in unique(data_stab[,"Target.Name"])) {
  
#  datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
  
  
#  plots[[val]] <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
#    geom_bar(stat="identity", position=position_dodge(), color = 'black') +
#    geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
#    
#    scale_fill_manual(values = colours)+
#    theme_classic()+
#    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
#    theme(legend.position = "none", axis.title.x = element_blank()) +
#    labs(y = paste(val, " Relative Quantification"))+
#    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
#    geom_hline(yintercept=2, linetype="dashed", color = "black")
#}


#grid.arrange(grobs = plots, ncol = 4)
