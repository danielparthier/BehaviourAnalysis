library(data.table)
library(readr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(patchwork)

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

# Correct for inverse y coordinates
DataSet[, (grep("_y", colnames(DataSet))) := 
          (lapply(.SD, function(x){-x})), 
        .SDcols = grep("_y", colnames(DataSet))]

# Generate mouse table with bodyparts
MouseData <- DataSet[,.SD,
                     .SDcols = !(names(DataSet) %like% "likelihood") & (names(DataSet) %like% "nose|ear|tail|body")]

# Calculate centroid
MouseData[,centroid_x:=sum(.SD)/3,
          .SDcols = !(names(MouseData) %like% "nose") & (names(MouseData) %like% "_x"),
          by = bodyparts_coords][
            ,centroid_y:=sum(.SD)/3,
            .SDcols = !(names(MouseData) %like% "nose") & (names(MouseData) %like% "_y"),
            by = bodyparts_coords]

MouseData[,head_x:=sum(.SD)/2,
              .SDcols = (names(MouseData) %like% "ear") & (names(MouseData) %like% "_x"),
              by = bodyparts_coords][
                ,head_y:=sum(.SD)/2,
                .SDcols = (names(MouseData) %like% "ear") & (names(MouseData) %like% "_y"),
                by = bodyparts_coords]

# Angle
MouseData[,head_angle:=(atan2(x = (nose_x-head_x),y = (nose_y-head_y))),][
  ,body_angle:=(atan2(x = (head_x-tailbase_x),y = (head_y-tailbase_y))),][
    ,view_angle:=head_angle-body_angle,] 

# Calculate Speed/Distance travelled
MouseData[,InstDistance := sqrt((shift(centroid_x, type = "lead") - centroid_x)^2+abs(shift(centroid_y, type = "lead") - centroid_y)^2)][
  ,Speed := InstDistance/(1/FrameRate)][
    ,CumDist := cumsum(InstDistance)]

# Find Object location
ObjectSet <- data.table(x=data.table::melt(DataSet[,.SD,
                                                   .SDcols = !(names(DataSet) %like% "likelihood") & (names(DataSet) %like% "object") & (names(DataSet) %like% "x")])$value,
                        y = data.table::melt(DataSet[,.SD,
                                                     .SDcols = !(names(DataSet) %like% "likelihood") & (names(DataSet) %like% "object") & (names(DataSet) %like% "y")])$value)
ObjectSet$ObjectLoc <- kmeans(x = ObjectSet[,.(x,y)], centers = ObjectNumber)$cluster
ObjectCoord <- ObjectSet[,.(x=median(x),y=median(y)), by=ObjectLoc]

# Calculate angle difference from object to mouse view
if(ObjectNumber>0) {
  for(i in 1:ObjectNumber) {
    object_angle <- paste("object", i, "angle", sep = "_")
    object_head_angle <- paste("object", i, "head_angle", sep = "_")
    MouseData[,eval(object_angle):=(atan2(x = (ObjectCoord[ObjectLoc==i, x]-head_x),
                                          y = (ObjectCoord[ObjectLoc==i, y]-head_y))),][
                                            ,eval(object_head_angle):=get(object_angle)-head_angle,][
                                              get(object_head_angle) > pi, eval(object_head_angle):=get(object_head_angle)-2*pi,][
                                                get(object_head_angle) < -pi, eval(object_head_angle):=get(object_head_angle)+2*pi,]
    
    # Calculate object distance
    MouseData[,paste0("DistToObject",i):=sqrt((nose_x-ObjectCoord[ObjectLoc==i, x])^2+(nose_y-ObjectCoord[ObjectLoc==i, y])^2)]
  }
  
  # Generate distance table
  ObjectDistance <- data.table::melt.data.table(MouseData[,.SD,.SDcols = (names(MouseData) %like% "DistToObject")])
  ObjectDistance[,ObjectNr := gsub("DistToObject", "", variable)][,Time := rep(MouseData$bodyparts_coords/FrameRate, times = ObjectNumber)][,variable:=NULL]
  setnames(x = ObjectDistance, old = "value", new = "Distance")
}

# Generate Plots
SpeedPlot <- ggplot(data = MouseData, aes(x = head_x, y = head_y, colour = Speed))+
  geom_path()+
  scale_color_viridis()+
  labs(colour = "Speed\n(px/s)")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
  annotate("text", x = ObjectCoord$x, y = ObjectCoord$y, label = ObjectCoord$ObjectLoc)+
  theme_void()+
  theme(legend.title.align=0.5)

DensityPlot <- ggplot(data = MouseData, aes(x = head_x, y = head_y))+
  stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE, n = c(500,500))+
  scale_fill_viridis()+
  labs(fill = "Density")+
  geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
  annotate("text", x = ObjectCoord$x, y = ObjectCoord$y, label = ObjectCoord$ObjectLoc)+
  theme_void()+
  theme(legend.title.align=0.5)

# Plot angle to object given that there is an object
if(ObjectNumber>0) {
  ObjectAnglePlots <- lapply(X = 1:ObjectNumber, FUN = function(x){
    PlotName <- paste0("AnglePlotObject", x)
    AnglePlot <- ggplot(data = MouseData, aes(x = head_x, y = head_y, colour = get(paste0("object_",x,"_head_angle")) *180/pi))+
      geom_path(size=1, linejoin = "round", lineend = "round")+
      scale_color_gradientn(colours = rainbow(5), limits = c(-180,180), breaks = c(-180, -90, 0, 90, 180))+
      guides(colour = guide_colourbar(ticks = FALSE, label.position = "right", title.position = "top"))+
      labs(colour = "Angle\n(Degree)")+
      geom_point(data = ObjectCoord, aes(x = x, y = y), shape = 21, colour = "black", alpha = 0.5, size = 8, stroke = 2)+
      annotate("text", x = ObjectCoord$x, y = ObjectCoord$y, label = ObjectCoord$ObjectLoc)+
      theme_void()+
      theme(legend.title.align=0.5)
    assign(x = PlotName, value = AnglePlot)
    return(get(PlotName))
  })
  AnglePlot <- wrap_plots(ObjectAnglePlots)
}

SpeedPlotLine <- ggplot(data = MouseData, aes(x = bodyparts_coords/FrameRate, y = Speed))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Speed (px/s)")+
  xlab("Time (s)")+
  theme_classic()+
  theme(legend.title.align=0.5)

DistancePlotLine <- ggplot(data = MouseData, aes(x = bodyparts_coords/FrameRate, y = CumDist))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Distance Travelled (px)")+
  xlab("Time (s)")+
  theme_classic()+
  theme(legend.title.align=0.5)

ObjectDistancePlotLine <- ggplot(data = ObjectDistance, aes(x = Time, y = Distance, group = ObjectNr, colour = ObjectNr))+
  geom_line()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_viridis(discrete=TRUE)+
  labs(colour = "Object")+
  ylab("Distance to Object (px)")+
  xlab("Time (s)")+
  theme_classic()+
  theme(legend.title.align=0.5)

# Arrange Plots
MovementPlot <- (SpeedPlot | ObjectAnglePlots[[1]] | ObjectAnglePlots[[2]]) / ObjectDistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))
OutPutPlotMap <- SpeedPlot + DensityPlot + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 
OutPutPlotMovement <- SpeedPlotLine + DistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 

ggsave(plot = MovementPlot, filename = "Plots/MovementPlot.pdf", device = "pdf", width = 10.4, height = 6)
ggsave(plot = OutPutPlotMap, filename = "Plots/OutPutPlotMap.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = OutPutPlotMovement, filename = "Plots/OutPutPlotMovement.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = ObjectDistancePlotLine, filename = "Plots/ObjectDistancePlotLine.pdf", device = "pdf", width = 5, height = 3)
