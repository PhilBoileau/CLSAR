% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSexAgeCateg.R
\name{createSexAgeCateg}
\alias{createSexAgeCateg}
\title{Create a Vector of Age and Sex Categories}
\usage{
createSexAgeCateg(ageVec, sexVec = c(), breaks)
}
\arguments{
\item{ageVec}{Vector containing the age of participants.}

\item{sexVec}{Vector containing the sex of participants.}

\item{breaks}{Indicates the interval size in years of each category.
Currently, intervals of 5, 10 and 20 years are supported.}
}
\value{
This function returns a vector of age and sex, if specified,
categories.
}
\description{
This function will create a vector of the age and sex category of the
the participants. Before using this function, make sure that the IDs
associated with the sex and age vectors are indentically ordered. Note
that if the sex vector is omitted, an age category vector will be produced.
}
\examples{
sex <- c("M", "F", "F")
age <- c(40, 33, 34)
ageSexVec <- createSexAgeCateg(ageVec = age, sexVec = sex,
                               breaks = 5)
}
\author{
Phil Boileau, \email{philippe.boileau (at) rimuhc.ca}
}
