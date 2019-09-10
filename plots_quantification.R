data_CNPC = read.csv("C:/Users/eddie/Desktop/CNPC_combined.csv")

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

data_CNPC$Column<- factor(data_CNPC$Column)


data_CNPC$Line <- revalue(data_CNPC$Column, c("1" = "AJC001-5", "2" = "AJD002-3", "3" = "AJG001-C4", "4" = "AIW001-02", "5" = "AIW002-02", "6" = "NCRM1", "7" = "KYOU", "8" = "TD02", "9" = "TD03", "10" = "TD10", "11" = "3448", "12" = "3450"))
data_CNPC$Line <- factor(data_CNPC$Line, levels = c("NCRM1", "KYOU", "AIW002-02", "AJC001-5", "AJG001-C4", "TD02",  "3448", "3450", "AJD002-3","TD03","TD10",  "TD22"))

data_CNPC <- data_CNPC[,colSums(is.na(data_CNPC))<nrow(data_CNPC)]
data_CNPC <- data_CNPC[rowSums(is.na(data_CNPC)) == 0,]

data_CNPC$MediaGrown <- revalue(data_CNPC$Line, c("NCRM1"="mTeSR1", "KYOU"="mTeSR1", "3448"="E8", "3450"="E8", "AJD002-3"="E8", "TD22"="E8", "AIW002-02"="mTeSR1", "AJC001-5"="mTeSR1", "AJG001-C4"="mTeSR1", "TD02"="mTeSR1", "TD03" = "E8", "TD10"="E8"))

#data_CNPC <- data_CNPC[data_CNPC$MediaGrown==data_CNPC$Media,]

data_map2 <- data_CNPC[data_CNPC[,"Ch1"]=="MAP2",]


data_map2 <- data_map2[data_map2$Dapi.positive > 50,]

data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei < 0.99,]
data_map2 <- data_map2[data_map2$too.big...250. <425,]
data_map2 <- data_map2[data_map2$Ch1.positive_per_nuclei > 0.01,]

p <- ggplot(data_map2, aes(y = Ch1.positive_per_nuclei, x = Line, fill = Line)) +
      geom_boxplot()+
      scale_fill_manual(values = colours)+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      scale_y_continuous(labels = function(x) x*100) +
      theme_classic() +
      ylab("% MAP2 Positive") +
      xlab(element_blank())+
      theme(axis.text.x = element_blank())
  

p


data_nes <- data_CNPC[data_CNPC[,"Ch2"]=="nestin",]


data_nes <- data_nes[data_nes$Dapi.positive > 50,]

data_nes <- data_nes[data_nes$Ch2.positive_per_nuclei < 0.99,]
data_nes <- data_nes[data_nes$too.big...250. <425,]
data_nes <- data_nes[data_nes$Ch2.positive_per_nuclei > 0.01,]

p <- ggplot(data_nes, aes(y = Ch2.positive_per_nuclei, x = Line, fill = Line)) +
  geom_boxplot()+
  scale_fill_manual(values = colours)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  scale_y_continuous(labels = function(x) x*100) +
  theme_classic() +
  ylab("% Nestin Positive") +
  xlab(element_blank())+
  theme(axis.text.x = element_blank())


p

p <- ggplot(data_nes, aes(y = Ch2.positive_per_nuclei, x = too.big...250., color = Line)) +
  geom_point()+
  scale_color_manual(values = colours)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  theme_classic() 

#SOX9 
#CD44


data_sox <- data_CNPC[data_CNPC[,"Ch2"]=="SOX9",]


data_sox <- data_sox[data_sox$Dapi.positive > 50,]

data_sox <- data_sox[data_sox$Ch2.positive_per_nuclei < 0.99,]
data_sox <- data_sox[data_sox$too.big...250. <425,]
data_sox <- data_sox[data_sox$Ch2.positive_per_nuclei > 0.01,]

p <- ggplot(data_sox, aes(y = Ch2.positive_per_nuclei, x = Line, fill = Line)) +
  geom_boxplot()+
  scale_fill_manual(values = colours)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  scale_y_continuous(labels = function(x) x*100) +
  theme_classic() +
  ylab("% SOX9 Positive") +
  xlab(element_blank())+
  theme(axis.text.x = element_blank())


p

data_cd44 <- data_CNPC[data_CNPC[,"Ch3"]=="CD44",]


data_cd44 <- data_cd44[data_cd44$Dapi.positive > 50,]

data_cd44 <- data_cd44[data_cd44$Ch3.positive_per_nuclei < 0.99,]
data_cd44 <- data_cd44[data_cd44$too.big...250. <425,]
data_cd44 <- data_cd44[data_cd44$Ch3.positive_per_nuclei > 0.01,]

p <- ggplot(data_cd44, aes(y = Ch3.positive_per_nuclei, x = Line, fill = Line)) +
  geom_boxplot()+
  scale_fill_manual(values = colours)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  scale_y_continuous(labels = function(x) x*100) +
  theme_classic() +
  ylab("% CD44 Positive") +
  xlab(element_blank())+
  theme(axis.text.x = element_blank())


p


#4weeks
#%MAP2 positive
#%TUJ1 positive 
#BRN2 / MAP2 positive
#TBR1 / MAP2 positive