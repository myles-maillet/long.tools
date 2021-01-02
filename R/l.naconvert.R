#' Convert Missing Data Values to NA
#'
#' This function converts a value (e.g., -999) to NA, which is the required for the use of other functions in this package that involve missing data.
#' @param data dataset stored as dataframe
#' @param miss_value current value specifying missing data (e.g., -999, \code{NULL})
#' @keywords missing data
#' @return returns a dataframe with missing data values converted to \code{NA}
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @export
#' @examples
#' new_ds <- l.naconvert(data = ds, miss_value = -999)


l.naconvert <- function(data, miss_value){

  data[data == miss_value] <- NA
  return(data)

}



