#' Speed/Distance/Cumulative distance 
#'
#' This function calculates the speed, distance, and cumulative distance and add them as DataTable column.
#' @param CoordTable A table including coordinates of labels.
#' @param SpeedRef A string indicating the label used for computing speed.
#' @param Interval A double corresponding to sampling rate.
#' @param ReferenceColumn A string indicating the reference column.
#' @param Overwrite A bool indicating if output should be overwritten if it exists already (default = TRUE).
#' 
#' @return Modifies existing DataTable.
#' @export
DistSpeedCalc <- function(CoordTable,
                          SpeedRef,
                          Interval,
                          ReferenceColumn = "",
                          Overwrite = TRUE) {
  if(Overwrite) {
    SpeedInstDistRef <- paste0("InstDistance", SpeedRef)
    SpeedCumDistRef <- paste0("CumDist", SpeedRef)
    SpeedSpeedRef <- paste0("Speed", SpeedRef)
  } else {
    SpeedInstDistRef <- VariableNameCheck(DataTable = CoordTable, NameString = paste0("InstDistance", SpeedRef))
    SpeedCumDistRef <- VariableNameCheck(DataTable = CoordTable, NameString = paste0("CumDist", SpeedRef))
    SpeedSpeedRef <- VariableNameCheck(DataTable = CoordTable, NameString = paste0("Speed", SpeedRef))
  }

  CoordTable[, eval(SpeedInstDistRef) := sqrt((data.table::shift(get(paste0(SpeedRef, "_x")), type = "lead") - get(paste0(SpeedRef, "_x")))^2 +
                                                (data.table::shift(get(paste0(SpeedRef, "_y")),
                                                                   type = "lead") - get(paste0(SpeedRef, "_y")))^2), by = ReferenceColumn]
  CoordTable[
                                                                     , eval(SpeedCumDistRef) := cumsum(get(SpeedInstDistRef)), by=ReferenceColumn]
  CoordTable[
                                                                       , eval(SpeedSpeedRef) := get(SpeedInstDistRef)/Interval]
}
