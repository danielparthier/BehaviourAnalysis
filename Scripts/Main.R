# Main script to process data
library(magrittr)
library(BehaviouR)
FileName <- "RawData/20180903_Schmitz_PaS_B1_C1_NL_Day1_NOVEL_DSC001663DLC_resnet50_NOVEL_VIDEONov7shuffle1_100000.csv"
FrameRate <- 25/3
ObjectNumber <- 2

MouseBodyList <- list()
MouseBodyList$head <- c("ear", "nose")
MouseBodyList$body <- c("ear", "tail")

ObjectList <- list("object")

MouseDataTable <- DeepLabCutLoad(FileName = FileName,
                                 FrameRate = FrameRate,
                                 MouseLabels = MouseBodyList,
                                 ObjectLabels = ObjectList,
                                 ObjectNumber = 2)

MouseDataTable$DataTable %>% 
  DistSpeedCalc(SpeedRef = "bodyCentroid", Interval = 1/FrameRate) %>% 
  AddCentroid(CornerNames = list("ear"),
              ReferenceColumn = "frame",
              OutputName = "BetweenEars") %>%
  VectorLength(VectorStart = "tailbase",
               VectorEnd = "BetweenEars",
               OutputName = "BodyLength") %>%
  AngleCalc(VectorStart = "BetweenEars",
            VectorEnd = "nose",
            OutputName = "HeadAngle") %>%
  AngleCalc(VectorStart = "tailbase",
            VectorEnd = "BetweenEars",
            OutputName = "BodyAngle") 
MouseDataTable$DataTable %>% 
  AngleDiff(Angle1 = "BodyAngle",
            Angle2 = "HeadAngle",
            OutputName = "ViewAngle") #%>%
MouseDataTable$DataTable %>% 
  ObjectDistance(ObjectTable = MouseDataTable$ObjectTable,
                 ObjectLabels = ObjectList,
                 Ref = "headCentroid") %>% 
  ObjectAngle(ObjectTable = MouseDataTable$ObjectTable,
              ObjectLabels = ObjectList,
              Ref = "headCentroid") %>%
  VectorLength(VectorStart = "nose",
               VectorEnd = "BetweenEars",
               OutputName = "headLength") %>%
  AddCentroid(CornerNames = list("ear"),
              ReferenceColumn = "frame",
              OutputName = "BetweenEars")


AddCentroid(CoordTable = DLCTable$CoordTable, CornerNames = list("ear"), ReferenceColumn = "frame", OutputName = "middlepoint")
