% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{entropy}
\alias{entropy}
\title{Calculate the entropy}
\usage{
entropy(x)
}
\arguments{
\item{x}{a numeric vector.}
}
\value{
numeric. entropy
}
\description{
Calculate the Shannon's entropy.
}
\examples{
set.seed(123)
x <- sample(1:10, 20, replace = TRUE)

entropy(x)

}
