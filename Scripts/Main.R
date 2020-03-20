# Main script to process data
library(magrittr)
library(BehaviouR)
library(ggplot2)
FileName <- paste0("RawData/20180903_Schmitz_PaS_B1_C1_NL_",
                   "Day1_NOVEL_DSC001663DLC_resnet50_NOVEL_",
                   "VIDEONov7shuffle1_100000.csv")
FrameRate <- 25/3
ObjectNumber <- 2

# generate lists
MouseBodyList <- list()
MouseBodyList$head <- c("ear", "nose")
MouseBodyList$body <- c("ear", "tail")

ObjectList <- list("object")

# load data
MouseDataTable <- DeepLabCutLoad(FileName = FileName,
                                 FrameRate = FrameRate,
                                 MouseLabels = MouseBodyList,
                                 ObjectLabels = ObjectList,
                                 ObjectNumber = ObjectNumber,
                                 xScale = 1,
                                 yScale = 1)

# compute body motion properties
DistSpeedCalc(CoordTable = MouseDataTable$DataTable,
              SpeedRef = "bodyCentroid",
              Interval = 1/FrameRate)
AddCentroid(CoordTable = MouseDataTable$DataTable,
            CornerNames = list("ear"),
            ReferenceColumn = "frame",
            OutputName = "BetweenEars")
VectorLength(CoordTable = MouseDataTable$DataTable,
             VectorStart = "tailbase",
             VectorEnd = "BetweenEars",
             OutputName = "BodyLength")
AngleCalc(CoordTable = MouseDataTable$DataTable,
          VectorStart = "BetweenEars",
          VectorEnd = "nose",
          OutputName = "HeadAngle")
AngleCalc(CoordTable = MouseDataTable$DataTable,
          VectorStart = "tailbase",
          VectorEnd = "BetweenEars",
          OutputName = "BodyAngle")
AngleDiff(CoordTable = MouseDataTable$DataTable,
          Angle1 = "BodyAngle",
          Angle2 = "HeadAngle",
          OutputName = "ViewAngle")

# compute object related properties
ObjectDistance(CoordTable = MouseDataTable$DataTable,
               ObjectTable = MouseDataTable$ObjectTable,
               ObjectLabels = ObjectList,
               Ref = "headCentroid")
ObjectAngle(CoordTable = MouseDataTable$DataTable,
            ObjectTable = MouseDataTable$ObjectTable,
            ObjectLabels = ObjectList,
            Ref = "headCentroid")
VectorLength(CoordTable = MouseDataTable$DataTable,
             VectorStart = "nose",
             VectorEnd = "BetweenEars",
             OutputName = "headLength")
AddCentroid(CoordTable = MouseDataTable$DataTable,
            CornerNames = list("ear"),
            ReferenceColumn = "frame",
            OutputName = "BetweenEars")


SpeedPlot <- SpeedPlot(DataTable = MouseDataTable$DataTable,
                       Speed = "SpeedbodyCentroid",
                       x = "headCentroid_x",
                       y = "headCentroid_y",
                       ObjectTable = MouseDataTable$ObjectTable)

DensityPlot <- LocationPlot(DataTable = MouseDataTable$DataTable,
                         x = "headCentroid_x",
                         y = "headCentroid_y",
                         ObjectTable = MouseDataTable$ObjectTable,
                         Density = T)


if(ObjectNumber>0) {
  ObjectAnglePlots <- lapply(X = 1:ObjectNumber, FUN = function(objID) {
    AnglePlot(DataTable = MouseDataTable$DataTable,
              Angle = paste0("object_",objID,"_headCentroid_Angle"),
              x = "bodyCentroid_x",
              y = "bodyCentroid_y",
              ObjectTable = MouseDataTable$ObjectTable,
              colourScheme = "dark")
  })   
}


SpeedPlotLine <- SpeedPlot(DataTable = MouseDataTable$DataTable, Speed = "SpeedbodyCentroid")

DistancePlotLine <- ggplot(data = MouseDataTable$DataTable, aes(x = bodyparts_coords/FrameRate, y = CumDist))+
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