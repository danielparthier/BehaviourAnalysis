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
  ObjectLoc <- NULL
  x <- NULL
  y <- NULL
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
  ObjectLoc <- NULL
  x <- NULL
  y <- NULL
  for(i in 1:dim(ObjectTable)[1]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    AngleName <- VariableNameCheck(DataTable = CoordTable,
                                   NameString = paste0(ObjectName, "_", Ref, "_Angle"))
    objX <- ObjectTable[ObjectLoc==eval(ObjectName),x]
    objY <- ObjectTable[ObjectLoc==eval(ObjectName),y]
    CoordTable[,"tmp_x" := objX,][
      ,"tmp_y" := objY,]
    AngleCalc(CoordTable = CoordTable,
              VectorStart = Ref,
              VectorEnd = "tmp",
              OutputName = AngleName)
    CoordTable[,"tmp_x":= NULL,][
      ,"tmp_y":= NULL,]
  }
}


#' Calculate entry to object
#'
#' This function calculates the angle from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param DistanceRef A string indicating the distance reference column.
#' @param Length  double as distance cut-off for zone.
#' @param AngleInclusion A bool indicating if angles should be considered for entry detection.
#' @param AngleRef A string indicating angle reference column.
#' @param AngleRange A double as indicating the angle range (+/-).
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable.
#' @export
ZoneEntry <- function(CoordTable,
                      DistanceRef,
                      Length,
                      AngleInclusion = TRUE,
                      AngleRef = NULL,
                      AngleRange = pi*(40/360),
                      Overwrite = TRUE) {
  if(!data.table::is.data.table(CoordTable)) {
    stop("No valid CoordTable")
  }
  if(length(colnames(CoordTable)[colnames(CoordTable)==DistanceRef])!=1) {
    stop("No valid DistanceRef")
  }
  
  if(!Overwrite) {
    InAreaRef <- VariableNameCheck(DataTable = CoordTable, NameString = paste0(DistanceRef, "_inArea"))
  } else {
    InAreaRef <- paste0(DistanceRef, "_inArea")
  }
  
  EntryRef <- paste0(InAreaRef, "_entry")
  BlockRef <- paste0(InAreaRef, "_block")
  CoordTable[,eval(InAreaRef):=0,
             ][get(DistanceRef)<Length,eval(InAreaRef):=1,][
               ,eval(EntryRef):=ifelse(test = get(InAreaRef)-shift(get(InAreaRef))==1,yes = 1, no = 0),
               ][is.na(get(EntryRef)),eval(EntryRef):=0][
                 ,eval(BlockRef):=cumsum(get(EntryRef))*get(InAreaRef)]
  
  if(AngleInclusion & length(colnames(CoordTable)[colnames(CoordTable)==AngleRef])==1) {
    AnglePositiveRef <- paste0(InAreaRef, "_AnglePositve")
    CoordTable[,eval(AnglePositiveRef):=0,][
      get(InAreaRef)==1,eval(AnglePositiveRef) := ifelse(test = min(abs(get(AngleRef)))<AngleRange/2,
                                                         yes = 1,
                                                         no = 0),
      by=eval(BlockRef)][
        ,eval(InAreaRef):=get(InAreaRef)*get(AnglePositiveRef)][
          ,eval(EntryRef):=get(EntryRef)*get(AnglePositiveRef),][
            ,eval(BlockRef):=cumsum(get(EntryRef))*get(InAreaRef)][
              ,eval(AnglePositiveRef):=NULL,]
  } else if(AngleInclusion & sum(colnames(CoordTable)==AngleRef)!=1) {
    message("Angle reference conflict. Check reference string.\nContinue without angle.")
  }
}
