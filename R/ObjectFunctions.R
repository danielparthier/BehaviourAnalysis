#' Calculate distance to objects 
#'
#' This function will calculate the speed, distance, and cumulative distance and add them as DataTable column.
#' @param CoordTable A table including coordinates of labels.
#' @param ObjectTable A table including the objects.
#' @param ObjectLabels A vector string indicating the labels used for computing object distances
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
    CoordTable[,tmp_x:=ObjectTable[ObjectLoc==eval(ObjectName),x]][
      ,tmp_y:=ObjectTable[ObjectLoc==eval(ObjectName),y]]
    VectorLength(CoordTable = CoordTable,
                 VectorStart = "tmp",
                 VectorEnd = Ref,
                 OutputName = DistanceName)
    CoordTable[,tmp_x:=NULL][
      ,tmp_y:=NULL]
  }
}

# Calculate angle to objects
ObjectAngle <- function(CoordTable, ObjectTable, ObjectLabels, Ref) {
  for(i in 1:dim(ObjectTable)[1]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    AngleName <- VariableNameCheck(DataTable = CoordTable,
                                   NameString = paste0(ObjectName, "_", Ref, "_Angle"))
    CoordTable[,tmp_x:=ObjectTable[ObjectLoc==eval(ObjectName),x]][
      ,tmp_y:=ObjectTable[ObjectLoc==eval(ObjectName),y]]
    AngleCalc(CoordTable = CoordTable,
              VectorStart = "tmp",
              VectorEnd = Ref,
              OutputName = AngleName)
    CoordTable[,tmp_x:=NULL][
      ,tmp_y:=NULL]
  }
}

