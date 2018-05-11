#' @title Load CLSA Comprehensive and Tracking Datasets
#'
#' @description
#' Load the most recent comprehensive and tracking data sets. Note that
#' the .csv files must already be saved on your computer before using this
#' function. Make sure that the csv files containing the data are named 
#' as follows:
#' \itemize{
#'   \item comprehensive: cop3_2.csv
#'   \item tracking: tra3_3.csv  
#' }
#' 
#' @param path The path to the folder containging the comprehensive and
#' tracking .csv files  
#'
#' @return A list of the comprehensive and tracking CLSA datasets in the following order:
#' \enumerate{
#'   \item comprehensive
#'   \item tracking
#' }
#' 
#' @import utils
#' 
#' @export
#'
#' @example 
#' \dontrun{
#' dataList <- loadCLSAData(path = "C:/Users/Documents")
#' 
#' # tracking data set
#' cop3.2 <- dataList[[1]]
#' tra3.3 <- dataList[[2]]
#' }
loadCLSAData <- function(path){
  
  # load the data sets
  tra3.3 <- read.csv(paste(path, "/tra3_3.csv", sep = ""), na.string = "")
  cop3.2 <- read.csv(paste(path, "/cop3_2.csv", sep = ""), na.string = "")
  
  # order the data by entitiy ID
  tra3.3 <- tra3.3[order(tra3.3$entity_id),]
  cop3.2 <- cop3.2[order(cop3.2$entity_id),]
  
  # return the list of data
  dataList <- list(cop3.2, tra3.3) 
  return(dataList)

}