data_tri <- read.csv("C:/Users/eddie/Desktop/Trilineage.csv")
data_stab <- read.csv("C:/Users/eddie/Desktop/stability.csv")
data_plu <- read.csv("C:/Users/eddie/Desktop/pluripotency2.csv")

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

#make sure there is rel in data
data_tri$Sample.Name <- factor(data_tri$Sample.Name, levels = c("H9","AIW001-02", "AIW002-02", "AJC001-5", "AJD002-3", "AJG001-C4","#3448", "#3450" ,"TD02", "TD10", "TD22","NCRM1", "KYOU"))
data_plu$Sample.Name <- factor(data_plu$Sample.Name, levels = c("H9","AIW001-02", "AIW002-02", "AJC001-5", "AJD002-3", "AJG001-C4","3448", "3450" ,"TD02","TD03", "TD10", "TD22","NCRM1", "KYOU"))
data_stab$Sample.Name <- factor(data_stab$Sample.Name, levels = c("Control","AIW001-02", "AIW002-02", "AJC001-5", "AJD002-3", "AJG001-C4","#3448", "#3450" ,"TD02", "TD10", "TD03","NCRM1", "KYOU"))

data_plu <- data_plu[,colSums(is.na(data_plu))<nrow(data_plu)]
data_plu <- data_plu[rowSums(is.na(data_plu)) == 0,]

data_stab <- data_stab[,colSums(is.na(data_stab))<nrow(data_stab)]
data_stab <- data_stab[rowSums(is.na(data_stab)) == 0,]


library("plotly")
library("ggplot2")
library("gridExtra")

#p <- ggplot(data_stab, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
#  theme_classic() +
#  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.6) +
#  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.6, position=position_dodge(width = 0.75)) +
#  scale_fill_manual(values = colours)
#p

data_group1 <- data_stab

data_group1$Sample.Name <-  factor(data_group1$Sample.Name, levels =c("Control","AIW001-02", "AIW002-02", "AJC001-5", "AJD002-3", "AJG001-C4","#3448"))

data_group1 <- data_group1[,colSums(is.na(data_group1))<nrow(data_group1)]
data_group1 <- data_group1[rowSums(is.na(data_group1)) == 0,]

data_group2 <- data_stab

data_group2$Sample.Name <-  factor(data_group2$Sample.Name, levels =c("Normal", "#3450" ,"TD02", "TD10", "TD03","NCRM1", "KYOU"))

data_group2 <- data_group2[,colSums(is.na(data_group2))<nrow(data_group2)]
data_group2 <- data_group2[rowSums(is.na(data_group2)) == 0,]


p1 <- ggplot(data_group1, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.6) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
  scale_fill_manual(values = colours1)+
  labs(x = "Chromosome region", y = "Relative Quantification")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+ 
  geom_hline(yintercept=2, linetype="dashed", color = "black")


p2 <- ggplot(data_group2, aes(x=Target.Name, y=Rq.mean, fill=Sample.Name)) +
  theme_classic() +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.6) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.3, position=position_dodge(width = 0.75)) +
  scale_fill_manual(values = colours2)+
  labs(x = "Chromosome region", y = "Relative Quantification")+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=2, linetype="dashed", color = "black")

grid.arrange(p1, p2, ncol = 1)


val = "C-MYC"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p1 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

val = "KLF4"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p2 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

val = "NANOG"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p3 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

val = "OCT3-4"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p4 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

val = "SOX2"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p5 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

val = "ZFP42"
datatemp <- data_plu[data_plu[,"Target.Name"]==val,]
p6 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.rel, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.rel-Rq.rel.error, ymax=Rq.rel+Rq.rel.error), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Expression")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  geom_hline(yintercept=1, linetype="dashed", color = "black")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

val = "chr1"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p1 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr8"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p2 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr10"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p3 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr12"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p4 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr17"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p5 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr18"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p6 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chr20"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p7 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

val = "chrX"
datatemp <- data_stab[data_stab[,"Target.Name"]==val,]
p8 <- ggplot(datatemp, aes(x=Sample.Name, y=Rq.mean, fill=Sample.Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Rq.mean-Rq.SEM, ymax=Rq.mean+Rq.SEM), width=0.5, position=position_dodge()) +
  scale_fill_manual(values = colours)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none", axis.title.x = element_blank()) +
  labs(y = paste(val, " Relative Quantification")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)

#library(plotly)
#plot_ly(data_tri, x = ~Target.Name, y = ~Rq.rel, color = ~Sample.Name, colors = colours, type = 'bar')
#plot_ly(data_plu, x = ~Target.Name, y = ~Rq.rel, color = ~Sample.Name, colors = colours, type = 'bar')
#plot_ly(data_stab, x = ~Target.Name, y = ~Rq.mean, color = ~Sample.Name, colors = colours, type = 'bar')