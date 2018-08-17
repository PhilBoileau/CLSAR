#' @title Create Stratified Summary Tables show Priscilla
#'
#' @description
#' Creates a summary table stratified by a categorical variable. If being used for the generation of
#' a report, it is recommended to use the \code{\link[pander]{pander}} function to format the table. 
#' Supports the use of weighted data.
#'
#' @param vect The vector of data to be summarized. It should be a continuous variable.
#' @param categories The vector of categories. If left undefined, a simple summary of 
#'                    the vect vector will be performed.
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
summaryTable <- function(vect, categories = c(), catname = "", 
                         missingValues = c(), weights = c()){
  
  # rename the dataframe's columns
  if(catname == "")
    catname <- deparse(substitute(categories))
  
  # if there are no specified categories, then perform a simple summary
  if(length(categories) == 0){
    
    # check if there are weights
    if(length(weights) == 0){
      
      # get the summary
      vectSummary <- summary(vect[!(vect %in% missingValues) & !is.na(vect)])
      
      # compute the standard deviation
      vectSD <- sd(vect[!(vect %in% missingValues) & !is.na(vect)])
      
      # count the number of missing
      vectMiss <- sum(is.na(vect)) + sum(vect %in% missingValues)
      
      # count number of non-missing
      vectNOTMiss <- NROW(vect) - vectMiss
      
      # combine all of the above statistics in a table
      vectAll <- data.frame(t(c(vectSummary, vectSD, vectNOTMiss, vectMiss)))
      
    } else {
      
      # get the weighted quantiles
      vectQuant <- wtd.quantile(vect[!(vect %in% missingValues) & !is.na(vect)],
                                weights = weights[!is.na(vect) & !(vect %in% missingValues)])
      
      # get the weighted mean
      wtdMean <- wtd.mean(vect[!(vect %in% missingValues) & !is.na(vect)],
                          weights = weights[!is.na(vect) & !(vect %in% missingValues)])
      
      # get the weighted std
      wtdSD <- sqrt(wtd.var(vect[!(vect %in% missingValues) & !is.na(vect)],
                       weights = weights[!is.na(vect) & !(vect %in% missingValues)]))
      
      # get sum of weights for missing data
      wtdMiss <- sum(weights[is.na(vect) | (vect %in% missingValues)])
      
      # get the sum of weights for the non-missing data
      wtdNOTMiss <- sum(weights[!is.na(vect) & !(vect %in% missingValues)])
      
      # combine the weighted statistics
      vectAll <- data.frame(t(c(vectQuant[1:3], wtdMean, vectQuant[4:5],
                                  wtdSD, wtdNOTMiss, wtdMiss)))
    }
    
  } else {
    
    # if there is more than one category
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
                                         FUN = (function(x){NROW(x) - sum(is.na(x)) - sum(x %in% missingValues)})))
    
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
  }
  
  # name the columns in the table
  if(length(categories) == 0)
    colnames(vectAll) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max",
                         "SD", "# Observations", "# Missing")
  else
    colnames(vectAll) <- c(catname, "Min", "Q1", "Median", "Mean", "Q3", "Max",
                           "SD", "# Observations", "# Missing")

  # return the dataframe
  return(vectAll)

}
