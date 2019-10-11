data <- read.csv("/home/bic/rthomas/Desktop/Link to 12CellLinesPaper/data/NPC_Sept10_output/combined.csv")


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



data_nu$Sample.Name <- factor(data_nu$Sample.Name, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02", "3448", "3450", "AJD002-3", "TD03","TD10",  "TD22"))

data_nu <- data_nu[,colSums(is.na(data_nu))<nrow(data_nu)]
data_nu <- data_nu[rowSums(is.na(data_nu)) == 0,]


plots <- list()

for (val in c("MAP2", "Tuj","NeuN", "STAB2", "S100B", "NCAM1", "PAX6" )) {
  data_temp <- data_nu[data_nu[,"Target.Name"]==val,]
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
