#' Compare a value to NA
#'
#' @param v1 Variable 1
#' @param v2 Variable 2
#'
#' @return TRUE if v1 or v2 are both non-NA or NA. FALSE if they do ot match.
#' @export
#' 
compareNA <- function(v1, v2) {
  
  # returns TRUE if v1 and v2 are equal, NA otherwise
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  
  # changes NA entry to FALSE
  if(is.na(same))
    same <- FALSE
  
  # return boolean
  return(same)
}
