#' @title Check if Two Values are NAs
#'
#' @description
#' Compare two values to determine if they are identical. This function
#' supports the comparison of NA values, which base R does not. Note that
#' vectors can also be compared, if they are the same length.
#'
#' @param v1 Value 1
#' @param v2 Value 2
#' 
#' @author Geva Maimon, \email{geva.maimon (at) rimuhc.ca}
#' 
#' @export
#' @return "TRUE" will be returned in the values are identical
#'          and "FALSE" will be returned otherwise.
#' 
#' @examples
#' compareNA(1, 1)
#' compareNA(1, 2)
#' compareNA(1, NA)
#' compareNA(NA, NA)
#' compareNA(1:5, 1:5)
#' compareNA(1:5, c(1:4, 6))
#' 
compareNA <- function(v1, v2) {
  
  # returns TRUE if v1 and v2 are equal, NA otherwise
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  
  # changes NA entry to FALSE
  same[is.na(same)] <- FALSE
  
  # return boolean(s)
  return(same)
}
