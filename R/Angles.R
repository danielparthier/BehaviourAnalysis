# Function to calculate angle (radians) between 2 vectors
#' Calculate angle between the reference frame and a vector or two vectors (optional).
#' 
#' This function will calculate the angle between two vectors. If only one vector is provided (VectorStart1, VectorEnd1)
#' then the angle in relation to the reference framework is calculated (0,0). If a second vector is provided the output
#' angle will be the difference between the two.
#' 
#' @param CoordTable A table including coordinates of labels.
#' @param VectorStart1 A string indicating the vector used for computing.
#' @param VectorEnd1 A string indicating the vector used for computing.
#' @param VectorStart2 A string indicating the vector used for computing (default = NULL).
#' @param VectorEnd2 A string indicating the vector used for computing (default = NULL).
#' @param OutputName A string indicating the label used for the output angle.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable.
#' @export
AngleCalc <- function(CoordTable,
                      VectorStart1,
                      VectorEnd1,
                      VectorStart2 = NULL,
                      VectorEnd2 = NULL,
                      OutputName,
                      Overwrite = TRUE) {
  tmpAng1 <- NULL
  tmpAng1 <- NULL
  if(sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorStart1, "_x"))))+sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorStart1, "_y"))))!= 2) {
    stop("reference not found for VectorStart")
  } else if(sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorEnd1, "_x"))))+sum(data.table::like(vector = names(CoordTable), pattern = c(paste0(VectorEnd1, "_y"))))!= 2) {
    stop("reference not found for VectorEnd")
  }
  if(!Overwrite) {
    OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = OutputName)
  }
  if(is.null(VectorStart2) | is.null(VectorEnd2)) {
    CoordTable[,eval(OutputName):=atan2(x = (get(paste0(VectorEnd1, "_x"))-get(paste0(VectorStart1, "_x"))),
                                        y = (get(paste0(VectorEnd1, "_y"))-get(paste0(VectorStart1, "_y")))),] 
  } else {
    CoordTable[,`:=`(tmpAng1=atan2(x = (get(paste0(VectorEnd1, "_x"))-get(paste0(VectorStart1, "_x"))),
                                     y = (get(paste0(VectorEnd1, "_y"))-get(paste0(VectorStart1, "_y")))),
                     tmpAng2=atan2(x = (get(paste0(VectorEnd2, "_x"))-get(paste0(VectorStart2, "_x"))),
                                     y = (get(paste0(VectorEnd2, "_y"))-get(paste0(VectorStart2, "_y"))))),]
    AngleDiff(CoordTable = CoordTable, Angle1 = "tmpAng1", Angle2 = "tmpAng2", OutputName = OutputName, Overwrite = T)
    CoordTable[,`:=`(tmpAng1=NULL,tmpAng2=NULL),]
  }
}

#' Calculate difference between two angles (radians)
#'
#' This function calculates the difference between two angles from a reference 
#' to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param Angle1 A double indicating the angle in radians.
#' @param Angle2 A double indicating the angle in radians.
#' @param OutputName A string indicating for the output angle difference.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
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

