#' Recode Chronic Conditions
#'
#' Note the mapping function: 1 = "Yes", 2 = "No",
#'  (8, 9, -8) = "DK/NoAn/REF/MIS"
#'
#' @param variable CCC or CCT variable that will be recoded
#' 
#' @import dplyr
#' @return Recoded variable
#' @export
#'
recodeChronicConditions <- function(variable){
  return(dplyr::recode_factor(variable, `1` = "Yes", `2` = "No",
                       `8` = "DK/NoAn/REF/MIS", `9` = "DK/NoAn/REF/MIS",
                       `-8` = "DK/NoAn/REF/MIS"))
}
