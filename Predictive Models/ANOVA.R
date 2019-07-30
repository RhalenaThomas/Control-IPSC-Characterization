##############################Score plots

sink("C:/Users/eddie/Documents/sink.csv")

#graphics.off()    

data4weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/4weeks_combined.csv")
data2weeks = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/2weeks_combined.csv")
dataCNPC = read.csv("C:/Users/eddie/Documents/Control-IPSC-Characterization/data/CNPC_combined.csv")

data4weeks$Time <-rep("4w",nrow(data4weeks))
data2weeks$Time <-rep("2w",nrow(data2weeks))
dataCNPC$Time <-rep("NPC",nrow(dataCNPC))

df <- rbind(rbind(data4weeks, data2weeks), dataCNPC)


var1_list <- c("Lines")   
var2_list <- c("Directory", "Media")
features <- c(
              "Ch1.positive_per_nuclei",
              "Dapi.positive",
              "size.average"
              
)

# Set seed for reproducibility
set.seed(42)


# load libraries




df$Well <- paste(df$Row, df$Column, df$Directory, df$Time) 

mean2 = function(x) {
  if (is.numeric(x[1]))
    return(mean(x))
  return(x[1])
}

df <- aggregate(df, by = list(df$Well), mean2)

df$Lines <- factor(df$Column)

data <- df[,colSums(is.na(df))<nrow(df)]
data <- data[rowSums(is.na(data)) == 0,]


for (var1 in var1_list) {
  for (val in unique(data[,var1])){
    
    datatemp <- data[data[,var1]==val,]
    
    for (var2 in var2_list){
      if (var1!= var2){
        for (Feature in features) {
          
          
          variables = paste(Feature, " ~ ", var2)
          print(val)
          print(variables)
          if (length(unique(datatemp[,var2])) > 1) {
            res.aov <- aov(as.formula(variables), data = datatemp)    
            #print(summary(res.aov))
            write.csv(as.matrix(res.aov), file = "ANOVA", na = "")
            print(TukeyHSD(res.aov))
            p <- ggplot(datatemp, aes_string(x = var1, y = Feature, fill= var2)) +
              geom_boxplot()
            print(p)
          } else {
            print("Less than two factors in vector")
          }
        }
      }
    }
  }
}



