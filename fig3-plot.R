data_stab <- read.csv("C:/Users/eddie/Desktop/stability.csv")

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

data_stab$Sample.Name <- factor(data_stab$Sample.Name, levels = c("Control", "NCRM1", "KYOU", "#3448", "#3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "TD10"))



data_stab <- data_stab[,colSums(is.na(data_stab))<nrow(data_stab)]
data_stab <- data_stab[rowSums(is.na(data_stab)) == 0,]



library(plyr)
library(gtable)
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



