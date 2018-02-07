#' Create a vector of age and sex categories
#'
#' @param ageVec Vector containing the age of participants.
#'               Make sure the ID order is identical to sexVec.
#' @param sexVec Vector containing the gender of participants.
#'               Make sure the ID order is identical to ageVec.
#' @param categoryNum Number of categories of each gender to create.
#'                    Currently supports options for categories of
#'                    size 8 or 16.
#'
#' @return Vector of age and sex category by ID 
#' @export
#'
createSexAgeCateg <- function(ageVec, sexVec, categoryNum){
  
  # ensure that the right number data is entered:
  if(length(ageVec) != length(sexVec))
    stop("Please ensure that ageVec and sexVec are of the same length")
  else if(categoryNum != 8 && categoryNum != 16)
    stop("Please select a categoryNum of either 8 or 16.")
  
  # intialize the age/sex category vector
  ageSex <- rep(NA, length(ageVec))
  
  # if user requests 8 categories
  if(categoryNum == 8){
    
    # get the age category
    ageCat <- sapply(ageVec, function(x){
      age <- "75-85"
      if(x < 55) {age <-"45-54"}
      else if(x < 65) {age <-"55-64"}
      else if(x< 75) {age <-"65-74"}
      return(age)
    })
    # fill the age and sex category vector
    ageSex <- sapply(1:length(ageSex), function(x) paste(ageCat[x], sexVec[x], sep = "")) 
  
    
  # if user requests 16 categories
  } else if(categoryNum == 16){
    
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
    # fill the age and sex category vector
    ageSex <- sapply(1:length(ageSex), function(x) paste(ageCat[x], sexVec[x], sep = "")) 
  }
  
  # return the category vector
  return(ageSex)
}
