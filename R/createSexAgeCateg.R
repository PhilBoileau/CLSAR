#' @title Create a Vector of Age and Sex Categories
#' 
#' @description
#' This function will create a vector of the age and sex category of the
#' the participants. Before using this function, make sure that the IDs
#' associated with the sex and age vectors are indentically ordered. Note
#' that if the sex vector is omitted, an age category vector will be produced.
#'
#' @param ageVec Vector containing the age of participants.
#' @param sexVec Vector containing the sex of participants.
#' @param breaks Indicates the interval size in years of each category.
#'               Currently, intervals of 5, 10 and 20 years are supported.
#'
#' @author Phil Boileau, \email{philippe.boileau (at) rimuhc.ca}
#' 
#' @export
#' @return This function returns a vector of age and sex, if specified,
#'         categories.
#'
#' @examples
#' sex <- c("M", "F", "F")
#' age <- c(40, 33, 34)
#' ageSexVec <- createSexAgeCateg(ageVec = age, sexVec = sex,
#'                                breaks = 5)
createSexAgeCateg <- function(ageVec, sexVec = c(), breaks){
  
  # ensure that the right number data is entered:
  if(length(ageVec) != length(sexVec))
    stop("Please ensure that ageVec and sexVec are of the same length")
  else if(breaks != 5 && breaks != 10 && breaks != 20)
    stop("Please select a break value of either 5, 10, 20.")
  
  # intialize the age/sex category vector
  ageSex <- rep(NA, length(ageVec))
  
  # if user requests breaks of 10 years
  if(breaks == 10){
    
    # get the age category
    ageCat <- sapply(ageVec, function(x){
      age <- "75-85"
      if(x < 55) {age <-"45-54"}
      else if(x < 65) {age <-"55-64"}
      else if(x< 75) {age <-"65-74"}
      return(age)
    })
    
  # if user requests breaks of 5 years
  } else if(breaks == 5){
    
    # get the age category
    ageCat <- sapply(ageVec, function(x){
      age <- "80-85"
      if(x < 50) {age <-"45-49"}
      else if(x < 55) {age <-"50-54"}
      else if(x < 60) {age <-"55-59"}
      else if(x < 65) {age <-"60-64"}
      else if(x < 70) {age <-"65-69"}
      else if(x < 75) {age <-"70-74"}
      else if(x < 80) {age <-"75-79"}
      return(age)
    })
    
  } else if(breaks == 20){
    
    # get the age category
    ageCat <- sapply(ageVec, function(x){
      age <- "65-85"
      if(x < 65) {age <- "45-64"}
      return(age)
    })
  }
  
  # add the sex category to the age vector if a sex vector is specified
  if(length(sexVec) != 0)
    catVect <- sapply(1:length(ageSex), function(x) paste(ageCat[x], sexVec[x], sep = "")) 
  else
    catVect <- ageVec
  
  # return the category vector
  return(catVect)
}
