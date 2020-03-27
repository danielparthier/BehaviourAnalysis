#' Load data and generate coordinate table and object table
#'
#' This function generates a DataTable from a csv exported with DeepLabCut and computes parameters as Speed/Distance, and other object related features.
#' @param FileName Name of CSV file including the path.
#' @param FrameRate A double indicating the frame rate.
#' @param MouseLabels A vector string indicating the labels used.
#' @param ObjectLabels A string indicating the label used for object.
#' @param ObjectNumber An integer indicating the number of objects.
#' @param xScale A double representing the scaling factor.
#' @param yScale A double representing the scaling factor.
#' @param JumpCorrections A bool indicating correction of label jump artefacts.
#' @param interpWindow An integer determining the length of spline fitting area around artefact.
#' @param cutWindow An integer determining the length of cut-out around artefact.
#' @import data.table
#' 
#' @return Generate DataTable from CSV file
#' @export
DeepLabCutLoad <- function(FileName,
                           FrameRate,
                           MouseLabels,
                           ObjectLabels,
                           ObjectNumber,
                           xScale = 1,
                           yScale = 1,
                           JumpCorrections = T,
                           interpWindow = 21) {
  DataSet <- data.table::fread(file = FileName, skip = 2)
  LabelNames <- data.table::fread(file = FileName, nrows = 1)
  
  # Construct unique columns
  ColumnNames <- paste(LabelNames, colnames(DataSet), sep="_")
  colnames(DataSet) <- ColumnNames
  # Correct for inverse y coordinates
  DataSet[, (grep("_y", colnames(DataSet))) := 
            (lapply(.SD, function(x){-x*yScale})), 
          .SDcols = grep("_y", colnames(DataSet))]
  DataSet[, (grep("_x", colnames(DataSet))) := 
            (lapply(.SD, function(x){x*xScale})), 
          .SDcols = grep("_x", colnames(DataSet))]
  
  # Generate mouse table with bodyparts
  CoordTable <- DataSet[,.SD,
                        .SDcols = !data.table::like(vector = names(DataSet),
                                                    pattern = "likelihood",
                                                    ignore.case = T)
                        & data.table::like(vector = names(DataSet),
                                           pattern = paste0(c(unique(unlist(MouseLabels)), ColumnNames[1]), collapse = "|"),
                                           ignore.case = T)]
  # Rename to frame and calculate time
  data.table::setnames(x = CoordTable, old = ColumnNames[1], new = "frame")
  
  # correct for outliers and artefact jumps
  if(JumpCorrections) {
    CoordinateCols <- c(grep("_x", colnames(CoordTable)), grep("_y", colnames(CoordTable)))
    for(i in CoordinateCols) {
      TargetCol <- colnames(CoordTable)[i]
      CoordInterp(CoordTable = CoordTable,
                    CoordRef = TargetCol,
                    interpWindow = interpWindow) 
      }
    }
  
  CoordTable[,"Time":=frame/FrameRate,]
  CentroidCollect(CoordTable = CoordTable, MouseLabels = MouseLabels)
  
  if(ObjectNumber > 0) {
    # Find Object location
    ObjNames_x <- names(DataSet)[!data.table::like(vector = names(DataSet),
                                                   pattern = "likelihood")
                                 & data.table::like(vector = names(DataSet),
                                                    pattern = paste0(unique(unlist(ObjectLabels)), collapse = "|"))
                                 & data.table::like(vector = names(DataSet),
                                                    pattern = "_x")]
    
    ObjNames_y <- names(DataSet)[!data.table::like(vector = names(DataSet),
                                                   pattern = "likelihood")
                                 & data.table::like(vector = names(DataSet),
                                                    pattern = paste0(unique(unlist(ObjectLabels)), collapse = "|"))
                                 & data.table::like(vector = names(DataSet),
                                                    pattern = "_y")]
    # Generate object Location
    ObjectSet <- data.table::data.table(x = data.table::melt.data.table(data = DataSet[,.SD,
                                                                                       .SDcols = ObjNames_x],
                                                                        measure.vars = ObjNames_x)$value,
                                        y = data.table::melt.data.table(data = DataSet[,.SD,
                                                                                       .SDcols = ObjNames_y],
                                                                        measure.vars = ObjNames_y)$value)
    ObjectSet$ObjectLoc <- kmeans(x = ObjectSet[,.(x,y)], centers = ObjectNumber)$cluster
    ObjectSet$Names <- rep(x = unique(unlist(ObjectLabels)), each = dim(DataSet)[1]*ObjectNumber)
    ObjectSet[,ObjectLoc:=paste0(Names, "_", ObjectLoc)]
    
    # Generate object coordinate table
    ObjectCoord <- ObjectSet[,.(x=median(x),y=median(y)), by=ObjectLoc]
  }
  OutputTable <- list()
  OutputTable$DataTable <- CoordTable
  if(ObjectNumber > 0) {
    OutputTable$ObjectTable <- ObjectCoord
  }
  return(OutputTable)
}

##### formula for fitting t-distribution
fitDT <- function(par, x){
  -sum(dt(x = x-par[2], df = par[1], log = T))
}

##### Interpolation of coordinates
CoordInterp <- function(CoordTable, CoordRef, interpWindow = 21) {
  par <- suppressWarnings(nlminb(x = diff(CoordTable[,get(CoordRef)]),
                                 fitDT,
                                 start = c(1,0))$par)
  MissLoc <- (1:CoordTable[,.N])[dt(x = diff(CoordTable[,get(CoordRef)])-par[2], df = par[1], log = T) < -10]+1
  iteration <- 1
  cutWindow <- 0
  while(length(MissLoc)!=0 & !is.null(MissLoc) & iteration < 10) {
    MissLoc <- unique(as.vector(sapply(MissLoc, function(x) {seq(x-cutWindow, x+cutWindow, 1)})))
    MissLoc <- MissLoc[MissLoc>0&MissLoc<CoordTable[,.N]]
    jumpLength <- ifelse(test = floor(interpWindow-1)/2 > 4, yes = floor(interpWindow-1)/2, no = 21) 
    startPoint <- ifelse(test = MissLoc[1]-jumpLength<0, yes = 1, no = MissLoc[1]-jumpLength)
    endPoint <- ifelse(test = MissLoc[1]+jumpLength>CoordTable[,.N], yes = CoordTable[,.N], no = MissLoc[1]+jumpLength)
    for(i in seq_along(MissLoc)) {
      if (i<(length(MissLoc)) & (MissLoc[i+1]-MissLoc[i])<(interpWindow-1)/2) {
        next
      }
      endPoint <- MissLoc[i]+jumpLength
      extractIDX <- MissLoc[data.table::between(x = MissLoc, lower = startPoint, upper = endPoint)]
      tmpLoc <- MissLoc[data.table::between(x = MissLoc, lower = startPoint, upper = endPoint)]
      modelSpline <- splines::interpSpline(obj2 = CoordTable[(startPoint:endPoint)[!(startPoint:endPoint) %in% tmpLoc],get(CoordRef)],
                                           obj1 = (startPoint:endPoint)[!(startPoint:endPoint) %in% tmpLoc])
      CoordTable[tmpLoc, eval(CoordRef) := predict(object = modelSpline,
                                                   x = extractIDX)$y,]
      if(i!=length(MissLoc)) {
        startPoint <- MissLoc[i+1]-jumpLength
        endPoint <- MissLoc[i+1]+jumpLength
      }
    }
    par <- suppressWarnings(nlminb(x = diff(CoordTable[,get(CoordRef)]),
                                   fitDT,
                                   start = c(1,0))$par)
    MissLoc <- (1:CoordTable[,.N])[dt(x = diff(CoordTable[,get(CoordRef)])-par[2], df = par[1], log = T) < -10]+1
    iteration <- iteration+1
    cutWindow <- cutWindow+1
  }
  message(paste("Correction applied for",CoordRef, "\nCutting window:",iteration-1))
  if(iteration == 10) {
    warning(paste("Iterationn exceeded limit for", CoordRef))
  }
}