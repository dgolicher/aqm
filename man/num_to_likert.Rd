% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aqm.R
\name{num_to_likert}
\alias{num_to_likert}
\title{Converts numerical data on a scale of 1 to 5 to Likert text}
\usage{
num_to_likert(x)
}
\arguments{
\item{x}{}
}
\description{
The function returns an ordinal factor.
}
\examples{

x<-sample(1:5, 100,replace=TRUE)
x
num_to_likert(x)


}
