# Checks that variable names are not duplicated
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
