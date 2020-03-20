#' Calculate distance to objects 
#'
#' This function calculates the distance from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param ObjectTable A table including the objects.
#' @param ObjectLabels A vector string indicating the labels used for computing object distances.
#' @param Ref A string indicating the label used for object distance.
#'
#' @return Modifies existing DataTable.
#' @export
ObjectDistance <- function(CoordTable,
                           ObjectTable,
                           ObjectLabels,
                           Ref) {
  for(i in 1:dim(ObjectTable)[1]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    DistanceName <- VariableNameCheck(DataTable = CoordTable,
                                      NameString = paste0(ObjectName, "_", Ref, "_Distance"))
    objX <- ObjectTable[ObjectLoc==eval(ObjectName),x]
    objY <- ObjectTable[ObjectLoc==eval(ObjectName),y]
    CoordTable[,"tmp_x" := objX,][
      ,"tmp_y" := objY,]
    VectorLength(CoordTable = CoordTable,
                 VectorStart = "tmp",
                 VectorEnd = Ref,
                 OutputName = DistanceName)
    CoordTable[,"tmp_x":= NULL,][
      ,"tmp_y":= NULL,]
  }
}

#' Calculate angle to objects 
#'
#' This function calculates the angle from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param ObjectTable A table including the objects.
#' @param ObjectLabels A vector string indicating the labels used for computing object angle.
#' @param Ref A string indicating the label used for object angle.
#'
#' @return Modifies existing DataTable.
#' @export
ObjectAngle <- function(CoordTable,
                        ObjectTable,
                        ObjectLabels,
                        Ref) {
  for(i in 1:dim(ObjectTable)[1]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    AngleName <- VariableNameCheck(DataTable = CoordTable,
                                   NameString = paste0(ObjectName, "_", Ref, "_Angle"))
    objX <- ObjectTable[ObjectLoc==eval(ObjectName),x]
    objY <- ObjectTable[ObjectLoc==eval(ObjectName),y]
    CoordTable[,"tmp_x" := objX,][
      ,"tmp_y" := objY,]
    AngleCalc(CoordTable = CoordTable,
              VectorStart = "tmp",
              VectorEnd = Ref,
              OutputName = AngleName)
    CoordTable[,"tmp_x":= NULL,][
      ,"tmp_y":= NULL,]
  }
}

