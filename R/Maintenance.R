#' Name checking
#'
#' This function checks for duplicates of columns.
#' @param DataTable A table including coordinates of labels.
#' @param NameString A string indicating the label used for object distance.
#'
#' @return Modifies existing DataTable.
#' @export
VariableNameCheck <- function(DataTable,
                              NameString) {
  if(sum(names(DataTable)==NameString)) {
    i <- 1
    NewName <- paste0(NameString, "_", i)
    while(sum(names(DataTable)==NewName)) {
      i <- 1+i
      NewName <- paste0(NameString, "_", i)
    }
    return(NewName)
  } else {
    return(NameString)
  }
}
