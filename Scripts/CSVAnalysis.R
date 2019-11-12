library(data.table)
library(readr)
library(tidyr)
library(ggplot2)
library(viridis)
library(gridExtra)
FileName <- "C:/Users/danie/Downloads/20180903_Schmitz_PaS_B1_C1_NL_Day1_NOVEL_DSC001663DLC_resnet50_NOVEL_VIDEONov7shuffle1_100000.csv"
DataSet <- FileName %>% read_csv(skip = 2)
LabelNames <- FileName %>% read_csv(skip = 1) %>% colnames() %>% 
  stringr::str_split(pattern = "_") %>%
  sapply(function(x){x[1]}) 

DatSetNames <- DataSet %>% colnames() %>%  stringr::str_split(pattern = "_") %>%
  sapply(function(x){x[1]})

ColumnNames <- paste(LabelNames, DatSetNames, sep="_")
colnames(DataSet) <- ColumnNames
DataSet <- data.table(DataSet)

DataSet[,NoseSpeed := sqrt(abs(shift(nose_x, type = "lead") - nose_x)+abs(shift(nose_y, type = "lead") - nose_y))]

ObjectSet <- data.table(x=c(DataSet$object1_x, DataSet$object2_x), y = c(DataSet$object1_y, DataSet$object2_y))
ObjectCoord <- ObjectSet[,ObjectLoc := ifelse(test = x<mean(x), yes = "left", no = "right"),][,.(x=median(x),y=median(y)), by=ObjectLoc]

SpeedPlot <- ggplot(data = DataSet, aes(x = nose_x, y = nose_y, colour = NoseSpeed))+
  geom_path()+
  scale_color_viridis()+
  labs(colour = "Speed")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", size = 8, stroke = 2)+
  theme_void()


DensityPlot <- ggplot(data = DataSet, aes(x = nose_x, y = nose_y))+
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)+
  scale_fill_viridis()+
  labs(fill = "Density")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", size = 8, stroke = 2)+
  theme_void()

OutPutPlot <- grid.arrange(SpeedPlot, DensityPlot, ncol = 2)

ggsave(plot = OutPutPlot, filename = "C:/Users/danie/Documents/PhD/Data/R_Analysis/R analysis/Behaviour/BehaviourAnalysis/Plots/OutPutPlot.pdf", device = "pdf", width = 16, height = 6)
