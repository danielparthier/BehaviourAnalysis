library(data.table)
library(readr)
library(ggplot2)
library(viridis)
library(gridExtra)

# Information about recording
FileName <- "RawData/20180903_Schmitz_PaS_B1_C1_NL_Day1_NOVEL_DSC001663DLC_resnet50_NOVEL_VIDEONov7shuffle1_100000.csv"
FrameRate <- 25/3
ObjectNumber <- 2

# Load data
DataSet <- fread(file = FileName, skip = 2)
LabelNames <- fread(file = FileName, nrows = 1)

# Construct unique columns
ColumnNames <- paste(LabelNames, colnames(DataSet), sep="_")
colnames(DataSet) <- ColumnNames

# Calculate Speed/Distance Travelled
DataSet[,InstDistance := sqrt((shift(nose_x, type = "lead") - nose_x)^2+abs(shift(nose_y, type = "lead") - nose_y)^2)][,NoseSpeed := InstDistance/(1/FrameRate)][,CumDist := cumsum(InstDistance)]

# Find Object Location
ObjectSet <- data.table(x=c(DataSet$object1_x, DataSet$object2_x), y = c(DataSet$object1_y, DataSet$object2_y))
ObjectSet$ObjectLoc <- kmeans(x = ObjectSet[,.(x,y)], centers = ObjectNumber)$cluster
ObjectCoord <- ObjectSet[,.(x=median(x),y=median(y)), by=ObjectLoc]

DataSet[,DistToObject1:=sqrt((nose_x-ObjectCoord[ObjectLoc==1, x])^2+(nose_y-ObjectCoord[ObjectLoc==1, y])^2)][,DistToObject2:=sqrt((nose_x-ObjectCoord[ObjectLoc==2, x])^2+(nose_y-ObjectCoord[ObjectLoc==2, y])^2)]


ObjectDistance <- data.table(Time = rep(DataSet$bodyparts_coords/FrameRate, times = 2), Distance = c(DataSet$DistToObject1, DataSet$DistToObject2), ObjectLoc = rep(c("1","2"), each = length(DataSet$bodyparts_coords)))

SpeedPlot <- ggplot(data = DataSet, aes(x = nose_x, y = nose_y, colour = NoseSpeed))+
  geom_path()+
  scale_color_viridis()+
  labs(colour = "Speed (px/s)")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", size = 8, stroke = 2)+
  theme_void()

DensityPlot <- ggplot(data = DataSet, aes(x = nose_x, y = nose_y))+
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)+
  scale_fill_viridis()+
  labs(fill = "Density")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", size = 8, stroke = 2)+
  theme_void()

SpeedPlotLine <- ggplot(data = DataSet, aes(x = bodyparts_coords/FrameRate, y = NoseSpeed))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Speed (px/s)")+
  xlab("Time (s)")+
  theme_classic()

DistancePlotLine <- ggplot(data = DataSet, aes(x = bodyparts_coords/FrameRate, y = CumDist))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Distance Travelled (px)")+
  xlab("Time (s)")+
  theme_classic()

ObjectDistancePlotLine <- ggplot(data = ObjectDistance, aes(x = Time, y = Distance, group = ObjectLoc, colour = ObjectLoc))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_viridis(discrete=TRUE)+
  labs(colour = "Object")+
  ylab("Distance to Object (px)")+
  xlab("Time (s)")+
  theme_classic()

OutPutPlotMap <- grid.arrange(SpeedPlot, DensityPlot, ncol = 2)
OutPutPlotMovement <- grid.arrange(SpeedPlotLine, DistancePlotLine, ncol = 2)

ggsave(plot = OutPutPlotMap, filename = "Plots/OutPutPlotMap.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = OutPutPlotMovement, filename = "Plots/OutPutPlotMovement.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = ObjectDistancePlotLine, filename = "Plots/ObjectDistancePlotLine.pdf", device = "pdf", width = 5, height = 3)
