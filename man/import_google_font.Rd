% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{import_google_font}
\alias{import_google_font}
\title{Import Google Fonts}
\usage{
import_google_font(family)
}
\arguments{
\item{family}{character. font family name}
}
\value{
No return value. This function just loads Google Fonts.
}
\description{
Import google font to be used when drawing charts.
}
\details{
When attaching the dlookr package, use "Roboto Condensed" and 
"Noto Sans Korean" among Google fonts. And also loads "Liberation Sans Narrow"
and "NanumSquare" included in the package for offline environment.

If you want to use anything other than the 4 fonts that are loaded with 
the dlookr package, load the desired Google fonts with import_google_font().

dlookr recommends the following google fonts, both sans and condensed:
"IBM Plex Sans Condensed", "Encode Sans Condensed", "Barlow Condensed", 
"Saira Condensed", "Titillium Web", "Oswald", "PT Sans Narrow"

Korean fonts:
"Nanum Gothic", "Gothic A1"
}
