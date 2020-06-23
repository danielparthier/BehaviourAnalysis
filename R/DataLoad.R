#' Calculate centroid
#'
#' This function calculates the centroid of an object consisting of references provided
#' as string vector.
#' 
#' @param CoordTable DataTable with coordinates.
#' @param CornerNames A vector string with labels.
#' @param OutputName A string for output.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#' 
#' @return Add centroid coordinates
#' @export
AddCentroid <- function(CoordTable,
                        CornerNames,
                        OutputName,
                        Overwrite = TRUE) {
  if(!Overwrite) {
    OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = OutputName)
  }
  if(length(CornerNames)>1) {
    ColumnSearch <- paste0(unique(unlist(CornerNames)), collapse = "|")
  } else if(length(CornerNames)==1) {
    ColumnSearch <- unlist(CornerNames)
  } else {
    stop("no search term")
  }
  
    CoordTable[,paste0(OutputName, "_x"):=mean(unlist(.SD)),
               .SDcols = data.table::like(vector = names(CoordTable),
                                          pattern = ColumnSearch,
                                          ignore.case = T)
               & data.table::like(vector = names(CoordTable),
                                  pattern = "_x"),by=rownames(CoordTable)][
                 ,paste0(OutputName, "_y"):=mean(unlist(.SD)),
                 .SDcols = data.table::like(vector = names(CoordTable),
                                            pattern = ColumnSearch,
                                            ignore.case = T)
                 & data.table::like(vector = names(CoordTable),
                                    pattern = "_y"),by=rownames(CoordTable)]
}

#' Wrapper for centroid calculation
#'
#' This function is a wrapper for the \code{AddCentroid} and allows computation of 
#' centroids for multiple object groups provided by a list.
#' 
#' @param CoordTable DataTable with coordinates.
#' @param MouseLabels A list with string vectors for labels.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#' 
#' @return Add centroid coordinates
#' @export
CentroidCollect <- function(CoordTable,
                            MouseLabels,
                            Overwrite = TRUE){
  for(i in 1:length(MouseLabels)) {
    if(!Overwrite) {
      OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = paste0(names(MouseLabels)[i], "Centroid"))
    } else {
      OutputName <- paste0(names(MouseLabels)[i], "Centroid")
    }
    AddCentroid(CornerNames = MouseLabels[[i]],
                CoordTable = CoordTable,
                OutputName = OutputName)
  }
}

#' Load data and generate coordinate table and object table
#' 
#' This function generates a DataTable from a csv exported with DeepLabCut and 
#' computes parameters as Speed/Distance, and other object related features.
#' 
#' @param FileName Name of CSV file including the path.
#' @param FrameRate A double indicating the frame rate.
#' @param MouseLabels A vector string indicating the labels used.
#' @param ObjectLabels A string indicating the label used for object.
#' @param ObjectNumber An integer indicating the number of objects.
#' @param xScale A double representing the scaling factor.
#' @param yScale A double representing the scaling factor.
#' @param JumpCorrections A bool indicating correction of label jump artefacts.
#' @param interpWindow An integer determining the length of spline fitting area around artefact.
#' @param includeAll A bool indicating if other labels should be imported as well (default = TRUE).
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
                           interpWindow = 21,
                           includeAll = T) {
  frame <- NULL
  x <- NULL
  y <- NULL
  ObjectLoc <- NULL
  Names <- NULL
  DataSet <- data.table::fread(file = FileName, skip = 2)
  LabelNames <- data.table::fread(file = FileName, nrows = 1)
  # Construct unique columns
  ColumnNames <- paste(LabelNames, colnames(DataSet), sep="_")
  message("all labels:\n", paste0(unlist(strsplit(x = grep(pattern = "_likelihood", x = ColumnNames, value = T),
                                                      split = "_likelihood")), collapse = "\n"), "\n")
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
    
    if(length(unique(unlist(ObjectLabels)))!=ObjectNumber) {
      ObjectLabels <- list(unlist(strsplit(names(DataSet)[data.table::like(vector = names(DataSet), pattern = "likelihood") & data.table::like(vector = names(DataSet), pattern = paste0(unique(unlist(ObjectLabels)), collapse = "|"))], split = "_likelihood")))
    }
    
    ObjectSet$ObjectLoc <- stats::kmeans(x = ObjectSet[,list(x,y)], centers = ObjectNumber)$cluster
    ObjectSet$Names <- rep(x = unique(unlist(ObjectLabels)), each = dim(DataSet)[1])

    NameTransfer <- ObjectSet[,.N,by=.(ObjectLoc, Names)][,MaxVal:=ifelse(max(x = N)==N, TRUE, FALSE),by=ObjectLoc][,Names:=as.factor(Names),][MaxVal==TRUE,Names,]
    for(i in 1:ObjectNumber) {
      ObjectSet[ObjectLoc==i,ObjectLocNew:=NameTransfer[i],]
    }
    ObjectSet[,ObjectLoc:=ObjectLocNew,]
    # Generate object coordinate table
    ObjectCoord <- ObjectSet[,list(x=stats::median(x),y=stats::median(y)), by=ObjectLoc]
  }
  if(includeAll) {
    RestColumns <- DataSet[,.SD,
                          .SDcols = !data.table::like(vector = names(DataSet),
                                                      pattern = "likelihood",
                                                      ignore.case = T)
                          & !data.table::like(vector = names(DataSet),
                                             pattern = paste0(c(unique(unlist(MouseLabels))), collapse = "|"),
                                             ignore.case = T)
                          & !data.table::like(vector = names(DataSet),
                                             pattern = paste0(unique(unlist(ObjectLabels)), collapse = "|"),
                                             ignore.case = T)
                          | data.table::like(vector = names(DataSet),
                                              pattern = ColumnNames[1],
                                              ignore.case = T)]
    
    if(JumpCorrections) {
      CoordinateCols <- c(grep("_x", colnames(RestColumns)), grep("_y", colnames(RestColumns)))
      for(i in CoordinateCols) {
        TargetCol <- colnames(RestColumns)[i]
        CoordInterp(CoordTable = RestColumns,
                    CoordRef = TargetCol,
                    interpWindow = interpWindow) 
      }
    }
    # Rename to frame and calculate time
    data.table::setnames(x = RestColumns, old = ColumnNames[1], new = "frame")
    RestColumns[,"Time":=frame/FrameRate,]
  }
  
  OutputTable <- list()
  OutputTable$DataTable <- CoordTable
  if(ObjectNumber > 0) {
    OutputTable$ObjectTable <- ObjectCoord
  }
  if(includeAll) {
    OutputTable$AllTable <- RestColumns
  }
  return(OutputTable)
}

##### formula for fitting t-distribution
fitDT <- function(par, x){
  -sum(stats::dt(x = x-par[2], df = par[1], log = T))
}

##### Interpolation of coordinates
CoordInterp <- function(CoordTable, CoordRef, interpWindow = 21) {
  par <- suppressWarnings(stats::nlminb(x = diff(CoordTable[,get(CoordRef)]),
                                 fitDT,
                                 start = c(1,0))$par)
  MissLoc <- (1:CoordTable[,.N])[stats::dt(x = diff(CoordTable[,get(CoordRef)])-par[2], df = par[1], log = T) < -10]+1
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
      endPoint <- ifelse(test = (MissLoc[i]+jumpLength)>CoordTable[,.N,], yes = CoordTable[,.N,], no = (MissLoc[i]+jumpLength))
      extractIDX <- MissLoc[data.table::between(x = MissLoc, lower = startPoint, upper = endPoint)]
      tmpLoc <- MissLoc[data.table::between(x = MissLoc, lower = startPoint, upper = endPoint)]
      modelSpline <- splines::interpSpline(obj2 = CoordTable[(startPoint:endPoint)[!(startPoint:endPoint) %in% tmpLoc],get(CoordRef)],
                                           obj1 = (startPoint:endPoint)[!(startPoint:endPoint) %in% tmpLoc])
      CoordTable[tmpLoc, eval(CoordRef) := stats::predict(object = modelSpline,
                                                   x = extractIDX)$y,]
      if(i!=length(MissLoc)) {
        startPoint <- MissLoc[i+1]-jumpLength
        endPoint <- MissLoc[i+1]+jumpLength
      }
    }
    par <- suppressWarnings(stats::nlminb(x = diff(CoordTable[,get(CoordRef)]),
                                   fitDT,
                                   start = c(1,0))$par)
    MissLoc <- (1:CoordTable[,.N])[stats::dt(x = diff(CoordTable[,get(CoordRef)])-par[2], df = par[1], log = T) < -10]+1
    iteration <- iteration+1
    cutWindow <- cutWindow+1
  }
  if(length(MissLoc)!=0) {
    message(paste("Correction applied for",CoordRef, "\nCutting window:",iteration-1))
  }
    if(iteration == 10) {
    warning(paste("Iterationn exceeded limit for", CoordRef))
  }
}
