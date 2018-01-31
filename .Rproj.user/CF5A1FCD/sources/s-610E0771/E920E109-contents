#' Create stratified summary tables
#'
#' @param vect The vector of data to be summarized
#' @param categories The vector of categories
#' @param catname The name of the categories
#' @param missingValues Optionally, the vector containing the codes of the missing values.
#'
#' @return Matrix of stratified summaries
#' @export
#' @import stats
#'
#' @examples \dontrun{
#' summaryTable(tra3.2$COG_AFT_SCORE_2_TRM, tra3.2$SEX_ASK_TRM, "Gender")
#' # produces a summary table of COG_AFT_SCORE_2_TRM stratified by gender
#' }
summaryTable <- function(vect, categories, catname, missingValues = c()){

  # compute the summary of the vector values
  vectSummary <- as.matrix(aggregate(vect[!(vect %in% missingValues) & !is.na(vect)],
                                     by=list(categories[!(vect %in% missingValues) & !is.na(vect)]), FUN=summary))

  # compute the standard deviation of the vector
  vectSD <- as.matrix(aggregate(vect[!(vect %in% missingValues) & !is.na(vect)],
                                by=list(categories[!(vect %in% missingValues) & !is.na(vect)]), FUN=sd))

  # count the number of missing values in the vector
  vectMiss <- as.matrix(aggregate(vect, by=list(categories),
                                  FUN = (function(x){sum(is.na(x)) + sum(x %in% missingValues)})))

  # count the number of non-missing values in the vector
  vectNOTMiss <- as.matrix(aggregate(vect, by=list(categories),
                                     FUN = (function(x){length(x)-sum(is.na(x)) - sum(x %in% missingValues)})))

  # combine all four calculations
  vectAll <- cbind(vectSummary[, -8],vectSD[, 2],vectNOTMiss[, 2], vectMiss[, 2])
  colnames(vectAll)<-c(catname, "Min","Q1","Median", "Mean","Q3","Max","SD","# Observations", "# Missing")

  # return the dataframe
  return(vectAll)

}
