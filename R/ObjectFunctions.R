#' Calculate distance to objects 
#'
#' This function calculates the distance from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param ObjectTable A table including the objects.
#' @param Ref A string indicating the label used for object distance.
#' @param ReferenceColumn A string indicating the reference column (used for batch analysis).
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable to add the object distance.
#' @export
ObjectDistance <- function(CoordTable,
                           ObjectTable,
                           Ref,
                           ReferenceColumn = "",
                           Overwrite = TRUE) {
  ObjectLoc <- NULL
  x <- NULL
  y <- NULL
  for(i in 1:ObjectTable[,.N,by=ReferenceColumn][,unique(N),]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    if(!Overwrite) {
      DistanceName <- VariableNameCheck(DataTable = CoordTable,
                                        NameString = paste0(ObjectName, "_", Ref, "_Distance"))
    } else {
      DistanceName <-paste0(ObjectName, "_", Ref, "_Distance")
    }
    if(any(ReferenceColumn == colnames(CoordTable))) {
      CoordTable[,"tmp_x":=NaN]
      CoordTable[,"tmp_y":=NaN]
      for(j in ObjectTable[,unique(get("FileName")),]) {
        objX <- ObjectTable[get(ReferenceColumn)==j&ObjectLoc==eval(ObjectName),x]
        objY <- ObjectTable[get(ReferenceColumn)==j&ObjectLoc==eval(ObjectName),y]
        CoordTable[get(ReferenceColumn)==j,"tmp_x" := objX,][
          get(ReferenceColumn)==j,"tmp_y" := objY,]
      }
      VectorLength(CoordTable = CoordTable,
                   VectorStart = "tmp",
                   VectorEnd = Ref,
                   OutputName = DistanceName)
    } else {
      objX <- ObjectTable[ObjectLoc==eval(ObjectName),x]
      objY <- ObjectTable[ObjectLoc==eval(ObjectName),y]
      CoordTable[,"tmp_x" := objX,][
        ,"tmp_y" := objY,]
      VectorLength(CoordTable = CoordTable,
                   VectorStart = "tmp",
                   VectorEnd = Ref,
                   OutputName = DistanceName)
    }
  }
  CoordTable[,"tmp_x":= NULL,][
    ,"tmp_y":= NULL,]
}

#' Calculate angle to objects 
#'
#' This function calculates the angle from a reference to objects from an object table and adds the resulting distances to the DataTable as columns.
#' @param CoordTable A table including coordinates of labels.
#' @param ObjectTable A table including the objects.
#' @param Ref A string indicating the label used for object angle.
#' @param RefStart A string indicating the label used for object angle.
#' @param ReferenceColumn A string indicating the reference column (used for batch analysis).
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable to add the angle to an object.
#' @export
ObjectAngle <- function(CoordTable,
                        ObjectTable,
                        Ref,
                        RefStart = NULL,
                        ReferenceColumn = "",
                        Overwrite = TRUE) {
  ObjectLoc <- NULL
  x <- NULL
  y <- NULL
  for(i in 1:ObjectTable[,.N,by=ReferenceColumn][,unique(N),]) {
    ObjectName <- ObjectTable[i, ObjectLoc]
    if(!Overwrite) {
      AngleName <- VariableNameCheck(DataTable = CoordTable,
                                     NameString = paste0(ObjectName, "_", Ref, "_Angle"))
    } else {
      AngleName <-paste0(ObjectName, "_", Ref, "_Angle")
    }
    if(any(ReferenceColumn == colnames(CoordTable))) {
      CoordTable[,"tmp_x":=NaN]
      CoordTable[,"tmp_y":=NaN]
      for(j in ObjectTable[,unique(get("FileName")),]) {
        objX <- ObjectTable[get(ReferenceColumn)==j&ObjectLoc==eval(ObjectName),x]
        objY <- ObjectTable[get(ReferenceColumn)==j&ObjectLoc==eval(ObjectName),y]
        CoordTable[get(ReferenceColumn)==j,"tmp_x" := objX,][
          get(ReferenceColumn)==j,"tmp_y" := objY,]
      }
      AngleCalc(CoordTable = CoordTable,
                VectorStart = Ref,
                VectorEnd = "tmp",
                OutputName = AngleName)
    } else {
      objX <- ObjectTable[ObjectLoc==eval(ObjectName),x]
      objY <- ObjectTable[ObjectLoc==eval(ObjectName),y]
      CoordTable[,"tmp_x" := objX,][
        ,"tmp_y" := objY,]
      if(is.null(RefStart)) {
        AngleCalc(CoordTable = CoordTable,
                  VectorStart1 = Ref,
                  VectorEnd1 = "tmp",
                  OutputName = AngleName) 
      } else {
        AngleCalc(CoordTable = CoordTable,
                  VectorStart1 = Ref,
                  VectorEnd1 = "tmp",
                  VectorStart2 = RefStart,
                  VectorEnd2 = Ref,
                  OutputName = AngleName) 
      }
    }
  }
  CoordTable[,"tmp_x":= NULL,][
    ,"tmp_y":= NULL,]
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
#' @return Modifies existing DataTable and add the binary output for an entry, .
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
