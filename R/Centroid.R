#' Calculate centroid
#'
#' This function calculates the centroid of object with .
#' @param CoordTable DataTable with coordinates.
#' @param CornerNames A vector string with labels .
#' @param ReferenceColumn A string indicating the reference column.
#' @param OutputName A string for output.
#' @param Overwrite A bool indicating if ouput should be overwritten if it exists already (default = TRUE).
#' 
#' @return Add centroid coordinates
#' @export
AddCentroid <- function(CoordTable,
                        CornerNames,
                        ReferenceColumn,
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
  
  CornerCount <- sum(data.table::like(vector = names(CoordTable),
                                      pattern = ColumnSearch)
                     & data.table::like(vector = names(CoordTable),
                                        pattern = "_x|_y"))/2
  if(!(CornerCount %% 1)) {
    CoordTable[,paste0(OutputName, "_x"):=sum(.SD)/CornerCount,
               .SDcols = data.table::like(vector = names(CoordTable),
                                          pattern = ColumnSearch,
                                          ignore.case = T)
               & data.table::like(vector = names(CoordTable),
                                  pattern = "_x"),
               by = ReferenceColumn][
                 ,paste0(OutputName, "_y"):=sum(.SD)/CornerCount,
                 .SDcols = data.table::like(vector = names(CoordTable),
                                            pattern = ColumnSearch,
                                            ignore.case = T)
                 & data.table::like(vector = names(CoordTable),
                                    pattern = "_y"),
                 by = ReferenceColumn]
  } else if(CornerCount > 3) {
    warning(paste("more than than 3 points for centroid estimation:", CornerCount))
  } else {
    stop(paste("missing coordinate for centroid estimation:", CornerCount))
  }
}

#' Wrapper for centroid calculation
#'
#' This function calculates the centroid of object with .
#' @param CoordTable DataTable with coordinates.
#' @param MouseLabels A vector string with labels.
#' @param Overwrite A bool indicating if ouput should be overwritten if it exists already (default = TRUE).
#' 
#' @return Add centroid coordinates
#' @export
CentroidCollect <- function(CoordTable,
                            MouseLabels,
                            Overwrite = TRUE){
  for(i in 1:length(MouseLabels)) {
    if(!Overwrite) {
      OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = paste0(names(MouseBodyList)[i], "Centroid"))
    } else {
      OutputName <- paste0(names(MouseBodyList)[i], "Centroid")
    }
    AddCentroid(CornerNames = MouseBodyList[[i]],
                CoordTable = CoordTable,
                ReferenceColumn = "frame",
                OutputName = OutputName)
  }
}

