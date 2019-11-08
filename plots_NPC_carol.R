  
  library(plyr)
  library(gtable)
  library("ggplot2")
  library("gridExtra")
  library(grid)
  library(dplyr)
  
  data_CNPC = read.csv("C:/Users/eddie/downloads/combined_NPC.csv")
  
  colours = c(
    "#b1457b",
    "#5c3788",
    "#b8434e",
    "#36dee6")
  
  data_CNPC$Column<- factor(data_CNPC$Column)
  
  
  data_CNPC$Ch1.positive_per_nuclei = data_CNPC$Ch1.positive / data_CNPC$Dapi.positive
  data_CNPC$Ch2.positive_per_nuclei = data_CNPC$Ch2.positive / data_CNPC$Dapi.positive
  data_CNPC$Ch3.positive_per_nuclei = data_CNPC$Ch3.positive / data_CNPC$Dapi.positive
  
  data_CNPC$Ch1.positive_per_map = data_CNPC$Ch1.positive / data_CNPC$Ch1.positive
  data_CNPC$Ch2.positive_per_map = data_CNPC$Ch1.Ch2.positive / data_CNPC$Ch1.positive
  data_CNPC$Ch3.positive_per_map = data_CNPC$Ch1.Ch3.positive / data_CNPC$Ch1.positive
  

  data_CNPC$Line <- revalue(data_CNPC$Column, c("1" = "AJC001-5", "2" = "AJD002-3", "3" = "AJG001-C4", "4" = "AIW001-02", "5" = "AIW002-02", "6" = "NCRM1", "7" = "KYOU", "8" = "TD02", "9" = "TD03", "10" = "TD10", "11" = "3448", "12" = "3450"))
  #data_CNPC$Line <- factor(data_CNPC$Line, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02",  "3448", "3450", "AJD002-3","TD03","TD10",  "TD22"))
  data_CNPC$Line <- factor(data_CNPC$Line, levels = c("NCRM1", "AJG001-C4", "AJD002-3", "TD03"))
  
  
  data_CNPC <- data_CNPC[,colSums(is.na(data_CNPC))<nrow(data_CNPC)]
  data_CNPC <- data_CNPC[!is.na(data_CNPC$Line),]
  
  data_CNPC$MediaGrown <- revalue(data_CNPC$Line, c("NCRM1"="mTeSR1", "KYOU"="mTeSR1", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR1", "AJC001-5"="mTeSR1", "AJG001-C4"="mTeSR1", "TD02"="mTeSR1", "TD03" = "E8", "TD10"="E8"))
  
  data_CNPC <- data_CNPC[data_CNPC$MediaGrown==data_CNPC$Media,]
  
  data_CNPC$Media <- factor(data_CNPC$Media, levels = c("mTeSR1","E8"))
    
  
  data_map2 <- data_CNPC[data_CNPC[,"Ch1"]=="MAP2",]
  
  data_map2 <- data_map2[data_map2$Dapi.positive > 50,]
  
  data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei < 0.99,]
  data_map2 <- data_map2[data_map2$too.big...250. <425,]
  data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei > 0.01,]
  
  p1 <- ggplot(data_map2, aes(y = Ch1.positive_per_nuclei, x = Line, fill = Line)) +
        geom_boxplot()+
        scale_fill_manual(values = colours)+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        scale_y_continuous(labels = function(x) x*100) +
        theme_classic() +
        ylab("% MAP2 Positive") +
        xlab(element_blank())+
        theme(axis.text.x = element_blank())+
        facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
    
  
  
  data_nes <- data_CNPC[data_CNPC[,"Ch2"]=="nestin",]
  
  
  data_nes <- data_nes[data_nes$Dapi.positive > 50,]
  
  data_nes <- data_nes[data_nes$Ch2.positive_per_nuclei < 0.99,]
  data_nes <- data_nes[data_nes$too.big...250. <425,]
  data_nes <- data_nes[data_nes$Ch2.positive_per_nuclei > 0.01,]
  
  p2 <- ggplot(data_nes, aes(y = Ch2.positive_per_nuclei, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% Nestin Positive") +
    xlab(element_blank())+
    theme(axis.text.x = element_blank())+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  
  
  p <- ggplot(data_nes, aes(y = Ch2.positive_per_nuclei, x = too.big...250., color = Line)) +
    geom_point()+
    scale_color_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme_classic() 
  
  #SOX9 
  #CD44
  
  
  data_ki67 <- data_CNPC[data_CNPC[,"Ch2"]=="nestinki67",]
  
  
  data_ki67 <- data_ki67[data_ki67$Dapi.positive > 50,]
  
  data_ki67 <- data_ki67[data_ki67$Ch2.positive_per_nuclei < 0.99,]
  data_ki67 <- data_ki67[data_ki67$too.big...250. <425,]
  data_ki67 <- data_ki67[data_ki67$Ch2.positive_per_nuclei > 0.01,]
  
  p3 <- ggplot(data_ki67, aes(y = Ch2.positive_per_nuclei, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% ki67 Positive") +
    xlab(element_blank())+
    theme(axis.text.x = element_blank())+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  
  
  data_sox1 <- data_CNPC[data_CNPC[,"Ch3"]=="SOX1",]
  
  
  data_sox1 <- data_sox1[data_sox1$Dapi.positive > 50,]
  
  data_sox1 <- data_sox1[data_sox1$Ch3.positive_per_map < 0.99,]
  data_sox1 <- data_sox1[data_sox1$too.big...250. <425,]
  data_sox1 <- data_sox1[data_sox1$Ch3.positive_per_map > 0.01,]
  
  p4 <- ggplot(data_sox1, aes(y = Ch3.positive_per_map, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% Sox1 Positive") +
    xlab(element_blank())+
    theme(axis.text.x = element_blank())+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  
  
  
  
  p <- grid.arrange(p1,p2,p3,p4)
  
  p
  
  #4weeks
  #%MAP2 positive
  #%TUJ1 positive 
  #BRN2 / MAP2 positive
  #TBR1 / MAP2 positive