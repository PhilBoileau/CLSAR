#' @title Recode Categorical Variables
#' 
#' @description 
#' Automatically recode a categorical text variable with the help of the data dictionary.
#' 
#' @param varName The name of the categorical variable to be recoded.
#' @param varVect The vector of responses for the specified variable.
#' @param dataDict The "Categories" sheet of the data dictionary produced by Opal. The
#'  data dictionary is an Excel spreadsheet that contains the two following sheets:
#' "Variables" and "Categories".
#'  
#' @author Phil Boileau, \email{philippe.boileau (at) rimuhc.ca}
#' 
#' @return Returns a recoded vector of the categorical variable. Note that if the function
#'  replaces entries by NA, then the data dictionary is incomplete for the specified
#'  variable.
#' 
#' @export
#' 
#' @example
#' \dontrun{
#' library(xlxs)
#' 
#' dataSet <- read.csv("path_to_file.csv")
#' dataDict <- read.xlsx("path_to_data_dictionary.xlsx", sheetName = "Categories")
#' 
#' recodeVariable(varName = "countryOfBirth",
#'               varVect = cobVect,
#'               dataDict = dictionary.df)
#' }
recodeVariable <- function(varName, varVect, dataDict){
  
  # make sure that the variable vector is a vector, the variable name is a string
  # and that the dataDict is a data frame
  if(class(varName) != "character")
    stop("Please enter a string for varName")
  else if(!(is.vector(varVect)))
    stop("Please enter a vector for varVect")
  else if(class(dataDict) != "data.frame")
    stop("Please enter data frame for dataDict")
  
  # format the numeric codes in dataDict to match the codes in the variable
  # this is accomplished by removing leading zeros from dataDict$name
  formatCodes <- suppressWarnings(ifelse(is.na(as.numeric(as.character(dataDict$name))),
                                    as.character(dataDict$name),
                                    as.character(as.numeric(as.character(dataDict$name)))))
  
  # recode the vector's values
  recodedVar <- factor(varVect,
                       levels = formatCodes[which(dataDict$variable == varName)],
                       labels = dataDict$label.en[which(dataDict$variable == varName)])
  
  # return the recoded vector
  return(recodedVar)
  
}
