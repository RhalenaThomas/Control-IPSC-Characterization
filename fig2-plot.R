library(plyr)
library(gtable)
library("ggplot2")
library("gridExtra")
library(grid)

data_plu <- read.csv("C:/Users/eddie/Desktop/pluripotency2.csv")
data_abs <- read.csv("C:/Users/eddie/Desktop/Absorbtion.csv")
data_if <- read.csv("C:/Users/eddie/Desktop/sum-control-paper.csv")

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


data_plu$Sample.Name <- factor(data_plu$Sample.Name, levels = c("H9", "NCRM1", "KYOU", "3448", "3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD10"))

data_plu$Media <- revalue(data_plu$Sample.Name, c("H9"="Control", "NCRM1"="Control", "KYOU"="Control", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR", "AJC001-5"="mTeSR", "AJG001-C5"="mTeSR", "TD02"="mTeSR", "TD10"="mTeSR"))


data_plu <- data_plu[,colSums(is.na(data_plu))<nrow(data_plu)]
data_plu <- data_plu[rowSums(is.na(data_plu)) == 0,]






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
    theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x")
  #print(plots[[val]])
}


p <- grid_arrange_shared_legend(plots[[1]],plots[[2]],plots[[3]],plots[[4]], plots[[5]], plots[[6]], nrow = 2, ncol = 3, position = "right")

print(grid.draw(p))



data_abs$Time <- as.factor(data_abs$Time)

data_abs$Sample <- factor(data_abs$Sample, levels = c("3448", "3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD10"))


data_abs <- data_abs[,colSums(is.na(data_abs))<nrow(data_abs)]
data_abs <- data_abs[rowSums(is.na(data_abs)) == 0,]

#TD03??



p <- ggplot(data_abs, aes(x=Time, y=Abs, fill=Media)) +
  geom_errorbar(aes(ymin=Abs-SE, ymax=Abs+SE), width=0.75, position=position_dodge(), size = 1) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75, color = 'black', size = 0.5) +
  theme_classic()+
  labs(y = "Absorbtion (D540)") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))+
  facet_grid(~Sample, drop = TRUE, scales ="free_x")

print(p)

data_if$name <- factor(data_if$name, levels = c("H9", "NCRM1", "KYOU", "3448", "3450", "AJD002-3", "TD22", "AIW002-02", "AJC001-5", "AJG001-C5", "TD02", "TD03", "TD10"))

data_if <- data_if[,colSums(is.na(data_if))<nrow(data_if)]
data_if <- data_if[rowSums(is.na(data_if)) == 0,]



p <- ggplot(data_if, aes(x=col, y=mean, fill=col)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.75, position=position_dodge(), size = 1) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.75, color = 'black', size = 0.5) +
  theme_classic()+
  labs(y = "Sum") +
  theme(axis.text.x = element_blank())+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme(legend.text=element_text(size=rel(1.2)), legend.key.size= unit(1.2, "cm"))+
  facet_grid(~name, drop = TRUE, scales ="free_x") 

print(p)


