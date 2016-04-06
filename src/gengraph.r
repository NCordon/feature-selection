data <- data.frame(Algoritmo="NN3", Tasa.test=96.55597, 
                Tasa.train=96.55597, Tasa.red=0.00000,
                Tiempo.exec=0.00000, Dataset="Wdbc",
                stringsAsFactors = FALSE)


#("NN3", 96.55597, 96.55597, 0.00000, 0.00000, "Wdbc")
data <- rbind(data,c("NN3", 67.27778, 68.22222, 0.00000, 0.00000, "Movement Libras"))
data <- rbind(data,c("NN3", 63.15883, 63.31454, 0.00000, 0.00000, "Arrhythmia"))

data <- rbind(data,c("SFS", 94.09402,	96.52100,	0.84000, 0.17420, "Wdbc"))
data <- rbind(data,c("SFS", 64.88889,	72.55556, 0.89111, 1.01910, "Movement Libras"))
data <- rbind(data,c("SFS", 69.23002, 75.75816, 0.97905, 2.35900, "Arrhythmia"))


data <- rbind(data,c("BL", 95.64183,	97.64505,	0.48667,	0.23150, "Wdbc"))
data <- rbind(data,c("BL", 66.66667,	68.66667,	0.52111,	0.98410, "Movement Libras"))
data <- rbind(data,c("BL", 62.90217,	64.55756,	0.50000,	19.60270, "Arrhythmia"))

data <- rbind(data,c("ES", 95.50186,	98.59402,	0.50333,	30.45330, "Wdbc"))
data <- rbind(data,c("ES", 66.88889,	70.50000,	0.51556,	25.48140, "Movement Libras"))
data <- rbind(data,c("ES", 63.26085,	68.91590,	0.51462,373.45210, "Arrhythmia"))


data <- rbind(data,c("BT", 96.02792,	98.91055,	0.52667,	44.06440, "Wdbc"))
data <- rbind(data,c("BT", 67.38889,	74.11111,	0.53444,	70.71810, "Movement Libras"))
data <- rbind(data,c("BT", 63.57013,	72.54564,	0.53755,	514.00300, "Arrhythmia"))

data <- rbind(data,c("BText", 95.64196,	98.84025,	0.50667,	53.01520, "Wdbc"))
data <- rbind(data,c("BText", 67.44444,	72.05556,	0.49333,	99.33300, "Movement Libras"))
data <- rbind(data,c("BText", 62.79693,	69.58763,	0.47391,	3135.18590, "Arrhythmia"))

data <-data.frame(ordered(data$Algoritmo, levels=c("NN3","SFS","BL","ES","BT","BText")), as.numeric(data$Tasa.test), data$Dataset)
names(data) <- c("Algoritmo","Tasa.test", "Dataset")
library(ggplot2)
library(scales)
"Arrhythmia" -> dataset
#"Wdbc" -> dataset
#"Movement Libras" -> dataset

ggplot(data=data [data$Dataset==dataset,], aes(x=Algoritmo, y=Tasa.test)) +
      ylab("Tasa de clasificación")+
      xlab("")+
      coord_cartesian(ylim=c(min(data$Tasa.test[data$Dataset==dataset]),max(data$Tasa.test[data$Dataset==dataset])))+
      geom_bar(stat="identity", colour="black", position = "dodge", aes(fill=unique(data$Algoritmo))) +
      #scale_fill_discrete(breaks=c("NN3","SFS","BL","ES", "BT", "BText"), name="Algoritmo") +
      scale_fill_discrete(guide=FALSE)+
      theme(axis.text.y=element_text(size=12), 
            axis.text.x=element_text(size=12),
            axis.title.y=element_text(size=15))

