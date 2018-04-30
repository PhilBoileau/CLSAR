#' @title Create Stratified Summary Tables
#'
#' @description
#' Creates a summary table stratified by a categorical variable. If being used for the generation of
#' a report, it is recommended to use the \code{\link[pander]{pander}} function to format the table. 
#' Supports the use of weighted data.
#'
#' @param vect The vector of data to be summarized.
#' @param categories The vector of categories.
#' @param catname The name of the categories. Defaults to the name of the category vector.
#' @param missingValues An optional vector containing the codes of the missing values.
#' @param weights An optional vector containing the weights of the data.
#'
#' @author Phil Boileau, \email{philippe.boileau (at) rimuhc.ca}
#'
#' @return Returns a table of stratified summaries.
#' @export
#' @import stats Hmisc
#' 
#' @example
#' summaryTable(vect = mtcars$mpg, categories = mtcars$cyl,
#'              catname = "Cylinders")
summaryTable <- function(vect, categories, catname = "", 
                         missingValues = c(), weights = c()){
  
  # rename the dataframe's columns
  if(catname == "")
    catname <- deparse(substitute(categories))
  
  if(length(weights) == 0){  
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
                                       FUN = (function(x){length(x) - sum(is.na(x)) - sum(x %in% missingValues)})))
  
    # combine all four calculations
    vectAll <- cbind.data.frame(vectSummary[, -8],vectSD[, 2],vectNOTMiss[, 2], vectMiss[, 2])
    
  } else {
    
    # define the number of rows in the table
    numRows <- length(levels(as.factor(categories)))
    
    # count the number of missing variables
    vectMiss <- matrix(NA, ncol = 1, nrow = numRows)
    for(i in 1:numRows){
      vectMiss[i, ] <- sum(weights[levels(as.factor(categories))[i] == as.factor(categories) &
                                  (is.na(vect) | vect %in% missingValues)])
    }
    
    # remove the missing variables from the vector, weights and categories
    weights <- weights[!is.na(vect) & !(vect %in% missingValues)]
    categories <- categories[!is.na(vect) & !(vect %in% missingValues)]
    vect <- vect[!is.na(vect) & !(vect %in% missingValues)]
    
    # compute the summary of the vector values
    vectSummary <- matrix(NA, ncol = 5, nrow = numRows)
    for(i in 1:numRows){
      vectSummary[i, ] <- wtd.quantile(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                                       weights[levels(as.factor(categories))[i] == as.factor(categories)])
    }
    
    # compute the std of the vector values
    vectSD <- matrix(NA, ncol = 1, nrow = numRows)
    for (i in 1:numRows) {
      vectSD[i, ] <- sqrt(wtd.var(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                                  weights[levels(as.factor(categories))[i] == as.factor(categories)]))
    }
    
    # compute the mean of the vector
    vectMean <- matrix(NA, ncol = 1, nrow = numRows)
    for (i in 1:numRows) {
      vectMean[i, ] <- wtd.mean(vect[levels(as.factor(categories))[i] == as.factor(categories)],
                                weights[levels(as.factor(categories))[i] == as.factor(categories)])
    }
    
    # count the number of non-missing values in the vector
    vectNotMiss <- as.matrix(aggregate(weights, by=list(categories), FUN = sum))[, -1]
    
    # combine all calculations
    
    vectAll <- cbind(levels(as.factor(categories)), as.data.frame(vectSummary[, 1:3]),
                     vectMean, as.data.frame(vectSummary[,4:5]), vectSD, vectNotMiss, vectMiss)
  }
  
  colnames(vectAll)<-c(catname, "Min","Q1","Median", "Mean","Q3","Max","SD","# Observations", "# Missing")

  # return the dataframe
  return(vectAll)

}
