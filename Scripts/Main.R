# Main script to process data
library(BehaviouR)
library(ggplot2)
library(patchwork)

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
                                 yScale = 1,
                                 JumpCorrections = T)

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

# Plot functions
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
              x = "headCentroid_x",
              y = "headCentroid_y",
              ObjectTable = MouseDataTable$ObjectTable,
              colourScheme = "light")
  })   
}


SpeedPlotLine <- SpeedPlot(DataTable = MouseDataTable$DataTable,
                           Speed = "SpeedbodyCentroid")

DistancePlotLine <- DistancePlot(DataTable = MouseDataTable$DataTable,
                                 Distance = "CumDistbodyCentroid")

ObjectDistancePlotLine <- DistancePlot(DataTable = MouseDataTable$DataTable,
                                       Distance = "headCentroid_Distance",
                                       ObjectTable = MouseDataTable$ObjectTable,
                                       ObjectDistance = T)

RearingPlotLine <- LengthPlot(DataTable = MouseDataTable$DataTable,
                              Length = "BodyLength")

RearingPlot <- LengthPlot(DataTable = MouseDataTable$DataTable,
                          Length = "BodyLength",
                          x = "headCentroid_x",
                          y = "headCentroid_y",
                          ObjectTable = MouseDataTable$ObjectTable)

# Arrange Plots
if(ObjectNumber>0) {
  MovementPlot <- (SpeedPlot | ObjectAnglePlots[[1]] | ObjectAnglePlots[[2]]) / ObjectDistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24))
  ggsave(plot = MovementPlot, filename = "Plots/MovementPlotLight.jpeg", device = "jpeg", width = 10.4, height = 6)
}

OutPutPlotRearing <- RearingPlotLine + RearingPlot
OutPutPlotMap <- SpeedPlot + DensityPlot + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 
OutPutPlotMovement <- SpeedPlotLine + DistancePlotLine + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size=24)) 

ggsave(plot = OutPutPlotMap, filename = "Plots/OutPutPlotMap.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = OutPutPlotMovement, filename = "Plots/OutPutPlotMovement.pdf", device = "pdf", width = 10.4, height = 4)
ggsave(plot = ObjectDistancePlotLine, filename = "Plots/ObjectDistancePlotLine.pdf", device = "pdf", width = 5, height = 3)
ggsave(plot = OutPutPlotRearing, filename = "Plots/RearingPlot.pdf", device = "pdf", width = 5, height = 3)
