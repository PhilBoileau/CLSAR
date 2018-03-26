#' Load CLSA Data Sets
#'
#' Make sure that the csv files containing the data are as follows:  
#' tracking: tra3_3.csv  
#' comprehensive: cop3_2.csv  
#' MCQ TRM: Tracking_30minQv2_Baseline.csv  
#' MCQ COM: Comprehensive_30minQv2_Baseline.csv  
#'
#' @param path The path to the folder containging the data files  
#'
#' @return A list of 4 CLSA data sets, in the following order:
#' tracking, comprehensive, MCQ tracking and MCQ comprehensive
#' 
#' @import utils
#' 
#' @export
#'
loadCLSAData <- function(path){
  
  # load the data sets
  tra3.3 <- read.csv(paste(path, "/tra3_3.csv", sep = ""), na.string = "")
  cop3.2 <- read.csv(paste(path, "/cop3_2.csv", sep = ""), na.string = "")
  mcq.tra <- read.csv(paste(path, "/Tracking_30minQv2_Baseline.csv", sep = ""), na.string = "")
  mcq.cop <- read.csv(paste(path, "/Comprehensive_30minQv2_Baseline.csv", sep = ""), na.string = "")
  
  # order the data by entitiy ID
  tra3.3 <- tra3.3[order(tra3.3$entity_id),]
  cop3.2 <- cop3.2[order(cop3.2$entity_id),]
  mcq.tra <- mcq.tra[order(mcq.tra$entity_id),]
  mcq.cop <- mcq.cop[order(mcq.cop$entity_id),]
  
  # return the list of data
  dataList <- list(tra3.3, cop3.2, mcq.tra, mcq.cop) 
  return(dataList)

}