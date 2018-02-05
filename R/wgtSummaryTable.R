#' Create weighted, stratified summary tables
#'
#' @param vect The vector of data to be summarized
#' @param wgts The vector containing the weights
#' @param categories The vector of categories
#' @param catname The name of the categories
#' @param missingValues Optionally, the vector containing the codes of the missing values
#'
#' @return Matrix of stratified summaries
#' @export
#' @import stats Hmisc
#'
wgtSummaryTable <- function(vect, wgts, categories, catname, missingValues = c()){

  # define the number of rows in the table
  numRows <- length(levels(as.factor(categories)))
  
  # count the number of missing variables
  vectMiss <- matrix(NA, ncol = 1, nrow = numRows)
  for(i in 1:numRows){
    vectMiss[i, ] <- sum(wgts[levels(as.factor(categories))[i] == as.factor(categories) &
                                (is.na(vect) | vect %in% missingValues)])
  }
  
  # remove the missing variables from the vector, weights and categories
  wgts <- wgts[!is.na(vect) & !(vect %in% missingValues)]
  categories <- categories[!is.na(vect) & !(vect %in% missingValues)]
  vect <- vect[!is.na(vect) & !(vect %in% missingValues)]
  
  # compute the summary of the vector values
  vectSummary <- matrix(NA, ncol = 5, nrow = numRows)
  for(i in 1:numRows){
    vectSummary[i, ] <- wtd.quantile(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                                     wgts[levels(as.factor(categories))[i] == as.factor(categories)])
  }
  
  # compute the std of the vector values
  vectSD <- matrix(NA, ncol = 1, nrow = numRows)
  for (i in 1:numRows) {
    vectSD[i, ] <- sqrt(wtd.var(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                                wgts[levels(as.factor(categories))[i] == as.factor(categories)]))
  }
  
  # compute the mean of the vector
  vectMean <- matrix(NA, ncol = 1, nrow = numRows)
  for (i in 1:numRows) {
    vectMean[i, ] <- wtd.mean(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                              wgts[levels(as.factor(categories))[i] == as.factor(categories)])
  }

  # count the number of non-missing values in the vector
  vectNotMiss <- as.matrix(aggregate(wgts, by=list(categories), FUN = sum))[, -1]

  # combine all calculations
  
  vectAll <- cbind(levels(as.factor(categories)), as.data.frame(vectSummary[, 1:3]),
                   vectMean, as.data.frame(vectSummary[,4:5]), vectSD, vectNotMiss, vectMiss)
  colnames(vectAll)<-c(catname, "Min","Q1","Median", "Mean","Q3","Max","SD","# Observations", "# Missing")

  # return the dataframe
  return(vectAll)

}
