% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate.R
\name{plot.univar_category}
\alias{plot.univar_category}
\title{Visualize Information for an "univar_category" Object}
\usage{
\method{plot}{univar_category}(
  x,
  na.rm = TRUE,
  prompt = FALSE,
  typographic = TRUE,
  base_family = NULL,
  ...
)
}
\arguments{
\item{x}{an object of class "univar_category", usually, a result of a call to univar_category().}

\item{na.rm}{logical. Specifies whether to include NA when plotting bar plot. 
The default is FALSE, so plot NA.}

\item{prompt}{logical. The default value is FALSE. If there are multiple visualizations to be output, 
if this argument value is TRUE, a prompt is output each time.}

\item{typographic}{logical. Whether to apply focuses on typographic elements to ggplot2 visualization. 
The default is TRUE. if TRUE provides a base theme that focuses on typographic elements using hrbrthemes package.}

\item{base_family}{character. The name of the base font family to use 
for the visualization. If not specified, the font defined in dlookr is applied. (See details)}

\item{...}{arguments to be passed to methods, such as graphical parameters (see par).
However, it does not support all parameters.}
}
\description{
Visualize mosaics plot by attribute of univar_category class.
}
\details{
The base_family is selected from "Roboto Condensed", "Liberation Sans Narrow",
"NanumSquare", "Noto Sans Korean". If you want to use a different font, 
use it after loading the Google font with import_google_font().
}
\examples{
\donttest{
library(dplyr)

# Calculates the all categorical variables
all_var <- univar_category(heartfailure)

# Print univar_category class object
all_var

smoking <- univar_category(heartfailure, smoking)

# Print univar_category class object
smoking

# plot all variables
plot(all_var)

# plot smoking
plot(smoking)
}

}
\seealso{
\code{\link{univar_category}}, \code{\link{print.univar_category}}, \code{\link{summary.univar_category}}.
}
