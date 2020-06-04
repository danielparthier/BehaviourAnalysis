# Function to calculate angle (radians) between 2 vectors
#' Calculate angle between two vectors
#'
#' This function calculates the angle from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param VectorStart A table including the objects.
#' @param VectorEnd A vector string indicating the labels used for computing object angle.
#' @param OutputName A string indicating the label used for object angle.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable.
#' @export
AngleCalc <- function(CoordTable,
                      VectorStart,
                      VectorEnd,
                      OutputName,
                      Overwrite = TRUE) {
  if(sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorStart, "_x"))))+sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorStart, "_y"))))!= 2) {
    stop("reference not found for VectorStart")
  } else if(sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorEnd, "_x"))))+sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorEnd, "_y"))))!= 2) {
    stop("reference not found for VectorEnd")
  }
  if(!Overwrite) {
    OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = OutputName)
  }
  CoordTable[,eval(OutputName):=atan2(x = (get(paste0(VectorEnd, "_x"))-get(paste0(VectorStart, "_x"))),
                                      y = (get(paste0(VectorEnd, "_y"))-get(paste0(VectorStart, "_y")))),]
}

#' Calculate difference between two angles (radians)
#'
#' This function calculates the difference between two angles from a reference 
#' to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param Angle1 A double indicating the angle in radians.
#' @param Angle2 A double indicating the angle in radians.
#' @param OutputName A string indicating for the output angle difference.
#' @param Overwrite A bool indicating if ouput should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable.
#' @export
AngleDiff <- function(CoordTable,
                      Angle1,
                      Angle2,
                      OutputName,
                      Overwrite = TRUE) {
  tmp <- NULL
  if(sum(names(CoordTable) %in% Angle1) == 0) {
    stop("Angle1 not found")
  } else if(sum(names(CoordTable) %in% Angle2) == 0) {
    stop("Angle2 not found")
  }
  if(!Overwrite) {
    OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = OutputName)
  }
  CoordTable[,tmp := get(x = Angle1)-get(x = Angle2),][
    tmp > pi, tmp := (tmp - pi*2)][
      tmp < -pi, tmp:=(tmp + pi*2)][
        ,eval(OutputName):=tmp,][
          ,tmp:=NULL,]
}

