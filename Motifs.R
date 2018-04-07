
library(readr)
library(ggplot2)
library(plotly)
library(stringr)
library(DataLoader)
library(forecast)
library(tseries)
library(zoo)
library(xts)
library(fts)
library(MASS)
library(caret)
library(e1071)
library(dplyr)
#library(h2o)        # Awesome ML Library
library(timetk)     # Toolkit for working with time series in R
library(tidyquant)
library(anomalyDetection)
library(TSMining)
library(randomForest)
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
packageVersion('plotly')

#==================================Motif discovery=======================================

 data(BuildOperation)
 res.wcc <- Func.motif(ts = BuildOperation$WCC, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)

 
res.wcc <- Func.motif(ts = dipu.pac_data$SUM,global.norm = T,local.norm = F,window.size = 24,overlap = 0,w = 6,a = 5,mask.size = 5,max.dist.ratio = 1.2,count.ratio.1 = 1.1,count.ratio.2 = 1.1)

res.ahu <- Func.motif(ts = pac$SP12, global.norm = T, local.norm = F, window.size = 24, overlap = 0, w = 6, a = 5, mask.size = 5, max.dist.ratio = 1.2, count.ratio.1 = 1.1, count.ratio.2 = 1.1)


#Visualization
data.wcc <- Func.visual.SingleMotif(single.ts = pac$SP11, window.size = 24, motif.indices = res.wcc$Indices)
data.ahu <- Func.visual.SingleMotif(single.ts = pac$SP12, window.size = 24, motif.indices = res.ahu$Indices)

#Determine the total number of motifs discovered in the time series of WCC
n <- length(unique(data.wcc$data.1$Y))
#Make the plot
p<-ggplot(data = data.wcc$data.1) +  
  geom_line(aes(x = 1:dim(data.wcc$data.1)[1], y = X)) +
  geom_point(aes(x = 1:dim(data.wcc$data.1)[1], y = X, color=Y, shape=Y))+
  scale_shape_manual(values = seq(from = 1, to = n)) +
  guides(shape=guide_legend(nrow = 2)) +
  xlab("Time (15-min)") + ylab("Počet áut") +
  theme(panel.background=element_rect(fill = "white", colour = "black"),
        legend.position="top",
        legend.title=element_blank())

ggplotly(p)
