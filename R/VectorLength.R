#' Vector Length
#'
#' This function calculates the vector length of two points at any given point in time.
#' @param CoordTable A table including coordinates of labels.
#' @param VectorStart A string indicating the label used for start of vector.
#' @param VectorEnd A string indicating the label used for end of vector.
#' @param OutputName A string indicating the label used as output.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#'
#' @return Modifies existing DataTable.
#' @export
VectorLength <- function(CoordTable,
                         VectorStart,
                         VectorEnd,
                         OutputName,
                         Overwrite = TRUE) {
  if(Overwrite) {
    OutputName <- VariableNameCheck(DataTable = CoordTable, NameString = OutputName)
  }
  # Vector length for two different columns
  CoordTable[, eval(OutputName):= sqrt((get(paste0(VectorStart, "_x")) - get(paste0(VectorEnd, "_x")))^2 +
                                         (get(paste0(VectorStart, "_y")) - get(paste0(VectorEnd, "_y")))^2)]
}
