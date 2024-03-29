% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pps.R
\name{summary.pps}
\alias{summary.pps}
\title{Summarizing Predictive Power Score}
\usage{
\method{summary}{pps}(object, ...)
}
\arguments{
\item{object}{an object of class "pps", usually, a result of a call to pps().}

\item{...}{further arguments passed to or from other methods.}
}
\description{
print and summary method for "pps" class.
}
\details{
summary.pps compares the PPS by variables.
}
\examples{
library(dplyr)

# If you want to use this feature, you need to install the 'ppsr' package.
if (!requireNamespace("ppsr", quietly = TRUE)) {
  cat("If you want to use this feature, you need to install the 'ppsr' package.\n")
}

# pps type is generic ======================================
pps_generic <- pps(iris)
pps_generic

if (!is.null(pps_generic)) {
  # summary pps class 
  mat <- summary(pps_generic)
  mat
}

# pps type is target_by =====================================
##-----------------------------------------------------------
# If the target variable is a categorical variable
# Using dplyr
pps_cat <- iris \%>\% 
  target_by(Species) \%>\% 
  pps()

pps_cat

if (!is.null(pps_cat)) {
  # summary pps class 
  tab <- summary(pps_cat)
  tab
}

##-----------------------------------------------------------
# If the target variable is a numerical variable
num <- target_by(iris, Petal.Length)

pps_num <- pps(num)
pps_num

if (!is.null(pps_num)) {
  # summary pps class 
  tab <- summary(pps_num)
  tab
}

}
\seealso{
\code{\link{pps}}, \code{\link{plot.pps}}.
}
