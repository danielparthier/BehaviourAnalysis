#' Load data and generate coordinate table and object table
#'
#' This function generates a DataTable from a csv exported with DeepLabCut and computes parameters as Speed/Distance, and other object related features.
#' @param FileName Name of CSV file including the path.
#' @param FrameRate A double indicating the frame rate.
#' @param MouseLabels A vector string indicating the labels used.
#' @param ObjectLabels A string indicating the label used for object.
#' @param ObjectNumber An integer indicating the number of objects.
#' @import data.table
#' 
#' @return Generate DataTable from CSV file
#' @export
DeepLabCutLoad <- function(FileName,
                           FrameRate,
                           MouseLabels,
                           ObjectLabels,
                           ObjectNumber) {
  DataSet <- data.table::fread(file = FileName, skip = 2)
  LabelNames <- data.table::fread(file = FileName, nrows = 1)
  
  # Construct unique columns
  ColumnNames <- paste(LabelNames, colnames(DataSet), sep="_")
  colnames(DataSet) <- ColumnNames
  # Correct for inverse y coordinates
  DataSet[, (grep("_y", colnames(DataSet))) := 
            (lapply(.SD, function(x){-x})), 
          .SDcols = grep("_y", colnames(DataSet))]
  
  # Generate mouse table with bodyparts
  CoordTable <- DataSet[,.SD,
                        .SDcols = !data.table::like(vector = names(DataSet),
                                                    pattern = "likelihood",
                                                    ignore.case = T)
                        & data.table::like(vector = names(DataSet),
                                           pattern = paste0(c(unique(unlist(MouseLabels)), "bodyparts"), collapse = "|"),
                                           ignore.case = T)]
  # Rename to frame and calculate time
  data.table::setnames(x = CoordTable, old = "bodyparts_coords", new = "frame")
  CoordTable[,Time:=frame/FrameRate,]
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
