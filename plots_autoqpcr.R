data_tri <- read.csv("C:/Users/eddie/Desktop/Trilineage.csv")
data_stab <- read.csv("C:/Users/eddie/Desktop/stability.csv")
data_plu <- read.csv("C:/Users/eddie/Desktop/pluripotency.csv")
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
#make sure there is rel in data
data_tri$Sample.Name <- factor(data_tri$Sample.Name, levels = c("H9","AJC001-5", "AJD002-3", "AJG001-C4", "AIW001-02", "AIW002-02", "NCRM1", "KYOU", "TD02", "TD03", "TD10", "#3448", "#3450" ))
data_plu$Sample.Name <- factor(data_plu$Sample.Name, levels = c("H9", "AJC001-5", "AJD002-3", "AJG001-C4", "AIW001-02", "AIW002-02", "NCRM1", "KYOU", "TD02", "TD03", "TD10", "#3448", "#3450" ))
data_stab$Sample.Name <- factor(data_stab$Sample.Name, levels = c("H9", "AJC001-5", "AJD002-3", "AJG001-C4", "AIW001-02", "AIW002-02", "NCRM1", "KYOU", "TD02", "TD03", "TD10", "#3448", "#3450" ))
data_plu <- data_plu[,colSums(is.na(data_plu))<nrow(data_plu)]
data_plu <- data_plu[rowSums(is.na(data_plu)) == 0,]
#p <- ggplot(data_tri, aes(x=Target.Name, y=Rq.rel, fill=Sample.Name)) +
#  geom_bar(stat="identity", position=position_dodge()) +
#  scale_fill_manual(values = colours)+
#  theme_minimal()
#p
library("gridExtra")
val = "C-MYC"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p1 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle("C-MYC")
val = "KLF4"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p2 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle("KLF4")
val = "NANOG"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p3 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle("NANOG")
val = "OCT3/4"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p4 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle("OCT3/4")
val = "SOX2"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p5 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle("SOX2")
val = "ZFP42"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p6 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = "Relative Expression")+
  ggtitle("ZFP4")
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
val = "chr1"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p1 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr8"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p2 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr10"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p3 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr12"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p4 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr17"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p5 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr18"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p6 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chr20"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p7 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
val = "chrX"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p8 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(x = "Sample Name", y = "Relative Expression")+
  ggtitle(val)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)
library(plotly)
#plot_ly(data_tri, x = ~Target.Name, y = ~Rq.rel, color = ~Sample.Name, colors = colours, type = 'bar')
#plot_ly(data_plu, x = ~Target.Name, y = ~Rq.rel, color = ~Sample.Name, colors = colours, type = 'bar')
#plot_ly(data_stab, x = ~Target.Name, y = ~Rq.mean, color = ~Sample.Name, colors = colours, type = 'bar')