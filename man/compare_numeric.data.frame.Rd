% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare_numeric}
\alias{compare_numeric}
\alias{compare_numeric.data.frame}
\title{Compare numerical variables}
\usage{
compare_numeric(.data, ...)

\method{compare_numeric}{data.frame}(.data, ...)
}
\arguments{
\item{.data}{a data.frame or a \code{\link{tbl_df}}.}

\item{...}{one or more unquoted expressions separated by commas.
You can treat variable names like they are positions.
Positive values select variables; negative values to drop variables.
These arguments are automatically quoted and evaluated in a context where column names
represent column positions.
They support unquoting and splicing.}
}
\value{
An object of the class as compare based list.
The information to examine the relationship between numerical variables is as follows each components.
- correlation component : Pearson's correlation coefficient.
\itemize{
\item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
\item var2 : factor. The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
\item coef_corr : double. Pearson's correlation coefficient.
}

- linear component : linear model summaries
\itemize{
\item var1 : factor. The level of the first variable to compare. 'var1' is the name of the first variable to be compared.
\item var2 : factor.The level of the second variable to compare. 'var2' is the name of the second variable to be compared.
\item r.squared : double. The percent of variance explained by the model.
\item adj.r.squared : double. r.squared adjusted based on the degrees of freedom.
\item sigma : double. The square root of the estimated residual variance.
\item statistic : double. F-statistic.
\item p.value : double. p-value from the F test, describing whether the full regression is significant.
\item df : integer degrees of freedom.
\item logLik : double. the log-likelihood of data under the model.
\item AIC : double. the Akaike Information Criterion.
\item BIC : double. the Bayesian Information Criterion.
\item deviance : double. deviance.
\item df.residual : integer residual degrees of freedom.
}
}
\description{
The compare_numeric() compute information to examine the relationship 
between numerical variables.
}
\details{
It is important to understand the relationship between numerical variables in EDA.
compare_numeric() compares relations by pair combination of all numerical variables. 
and return compare_numeric class that based list object.
}
\section{Attributes of return object}{

Attributes of compare_numeric class is as follows.
\itemize{
\item raw : a data.frame or a \code{\link{tbl_df}}. Data containing variables to be compared. Save it for visualization with plot.compare_numeric().
\item variables : character. List of variables selected for comparison. 
\item combination : matrix. It consists of pairs of variables to compare.
}
}

\examples{
\donttest{
# Generate data for the example
heartfailure2 <- heartfailure[, c("platelets", "creatinine", "sodium")]

library(dplyr)
# Compare the all numerical variables
all_var <- compare_numeric(heartfailure2)

# Print compare_numeric class object
all_var

# Compare the correlation that case of joint the sodium variable
all_var \%>\% 
  "$"(correlation) \%>\% 
  filter(var1 == "sodium" | var2 == "sodium") \%>\% 
  arrange(desc(abs(coef_corr)))
  
# Compare the correlation that case of abs(coef_corr) > 0.1
all_var \%>\% 
  "$"(correlation) \%>\% 
  filter(abs(coef_corr) > 0.1)
  
# Compare the linear model that case of joint the sodium variable  
all_var \%>\% 
  "$"(linear) \%>\% 
  filter(var1 == "sodium" | var2 == "sodium") \%>\% 
  arrange(desc(r.squared))
  
# Compare the two numerical variables
two_var <- compare_numeric(heartfailure2, sodium, creatinine)

# Print compare_numeric class objects
two_var
  
# Summary the all case : Return a invisible copy of an object.
stat <- summary(all_var)

# Just correlation
summary(all_var, method = "correlation")

# Just correlation condition by r > 0.1
summary(all_var, method = "correlation", thres_corr = 0.1)

# linear model summaries condition by R^2 > 0.05
summary(all_var, thres_rs = 0.05)

# verbose is FALSE 
summary(all_var, verbose = FALSE)
  
# plot all pair of variables
plot(all_var)

# plot a pair of variables
plot(two_var)

# plot all pair of variables by prompt
plot(all_var, prompt = TRUE)

# plot a pair of variables not focuses on typographic elements
plot(two_var, typographic = FALSE)
}

}
\seealso{
\code{\link{correlate}}, \code{\link{summary.compare_numeric}}, \code{\link{print.compare_numeric}}, \code{\link{plot.compare_numeric}}.
}
