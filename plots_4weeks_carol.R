  
  library(plyr)
  library(gtable)
  library("ggplot2")
  library("gridExtra")
  library(grid)
  library(dplyr)
  
  data_4w = read.csv("C:/Users/eddie/downloads/combined_4weeks.csv")
  
  colours = c(
    "#54b06c",
    "#5c3788",
    "#b8434e",
    "#36dee6")
  
  data_4w$Column<- factor(data_4w$Column)
  
  
  data_4w$Ch1.positive_per_nuclei = data_4w$Ch1.positive / data_4w$Dapi.positive
  data_4w$Ch2.positive_per_nuclei = data_4w$Ch2.positive / data_4w$Dapi.positive
  data_4w$Ch3.positive_per_nuclei = data_4w$Ch3.positive / data_4w$Dapi.positive
  
  data_4w$Ch1.positive_per_map = data_4w$Ch1.positive / data_4w$Ch1.positive
  data_4w$Ch2.positive_per_map = data_4w$Ch1.Ch2.positive / data_4w$Ch1.positive
  data_4w$Ch3.positive_per_map = data_4w$Ch1.Ch3.positive / data_4w$Ch1.positive
  
  
  
  data_4w$Line <- revalue(data_4w$Column, c("1" = "AJC001-5", "2" = "AJD002-3", "3" = "AJG001-C4", "4" = "AIW001-02", "5" = "AIW002-02", "6" = "NCRM1", "7" = "KYOU", "8" = "TD02", "9" = "TD03", "10" = "TD10", "11" = "3448", "12" = "3450"))
  #data_4w$Line <- factor(data_4w$Line, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02",  "3448", "3450", "AJD002-3","TD03","TD10",  "TD22"))
  #data_4w$Line <- factor(data_4w$Line, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02",  "3448", "3450", "AJD002-3","TD03","TD10",  "TD22"))
  data_4w$Line <- factor(data_4w$Line, levels = c("KYOU","AJG001-C4", "AJD002-3", "TD03"))
  
  
  data_4w <- data_4w[,colSums(is.na(data_4w))<nrow(data_4w)]
  data_4w <- data_4w[rowSums(is.na(data_4w)) == 0,]
  
  data_4w$MediaGrown <- revalue(data_4w$Line, c("NCRM1"="mTeSR1", "KYOU"="mTeSR1", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR1", "AJC001-5"="mTeSR1", "AJG001-C4"="mTeSR1", "TD02"="mTeSR1", "TD03" = "E8", "TD10"="E8"))
  
  data_4w <- data_4w[data_4w$MediaGrown==data_4w$Media,]
  
  data_4w$Media <- factor(data_4w$Media, levels = c("mTeSR1","E8"))
  data_map2 <- data_4w[data_4w[,"Ch1"]=="MAP2",]
  
  
  data_map2 <- data_map2[data_map2$Dapi.positive > 50,]
  
  data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei < 0.99,]
  data_map2 <- data_map2[data_map2$too.big...250. <425,]
  data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei > 0.01,]
  
  p1 <- ggplot(data_map2, aes(y = Ch1.positive_per_nuclei, x = Line, fill = Line)) +
        geom_boxplot()+
        scale_fill_manual(values = colours)+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
        theme_classic() +
        ylab("% MAP2 Positive") +
        xlab(element_blank())+
        theme(axis.text.x = element_blank())+
        facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
    
  
  
  data_tuj <- data_4w[data_4w[,"Ch2"]=="Tuj1" | data_4w[,"Ch1"]=="Tuj1",]
  
  
  data_tuj <- data_tuj[data_tuj$Dapi.positive > 50,]
  
  data_tuj <- data_tuj[data_tuj$Ch2.positive_per_nuclei < 0.99,]
  data_tuj <- data_tuj[data_tuj$too.big...250. <425,]
  data_tuj <- data_tuj[data_tuj$Ch2.positive_per_nuclei > 0.01,]
  
  p2 <- ggplot(data_tuj, aes(y = Ch2.positive_per_nuclei, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% Tuj1 Positive") +
    xlab(element_blank())+
    theme(axis.text.x = element_blank())+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  
  
  
  
  p <- ggplot(data_tuj, aes(y = Ch2.positive_per_nuclei, x = too.big...250., color = Line)) +
    geom_point()+
    scale_color_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    theme_classic() 
  
  #SOX9 
  #CD44
  
  
  data_brn <- data_4w[data_4w[,"Ch3"]=="Brn2",]
  
  
  data_brn <- data_brn[data_brn$Dapi.positive > 50,]
  
  data_brn <- data_brn[data_brn$Ch2.positive_per_map < 0.99,]
  data_brn <- data_brn[data_brn$too.big...250. <425,]
  data_brn <- data_brn[data_brn$Ch2.positive_per_map > 0.01,]
  
  p3 <- ggplot(data_brn, aes(y = Ch3.positive_per_map, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% Brn2 Positive") +
    xlab(element_blank())+
    theme(axis.text.x = element_blank())+
    facet_grid(~Media ,drop = TRUE, scales = "free_x",  space = "free_x", switch = "x")
  
  
  
  data_tbr <- data_4w[data_4w[,"Ch3"]=="TBR1",]
  
  
  data_tbr <- data_tbr[data_tbr$Dapi.positive > 50,]
  
  data_tbr <- data_tbr[data_tbr$Ch3.positive_per_map < 0.99,]
  data_tbr <- data_tbr[data_tbr$too.big...250. <425,]
  data_tbr <- data_tbr[data_tbr$Ch3.positive_per_map > 0.01,]
  
  p4 <- ggplot(data_tbr, aes(y = Ch3.positive_per_map, x = Line, fill = Line)) +
    geom_boxplot()+
    scale_fill_manual(values = colours)+
    scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
    scale_y_continuous(labels = function(x) x*100, limits = c(0, 1)) +
    theme_classic() +
    ylab("% TBR1 Positive") +
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