% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{beard}
\alias{beard}
\title{Beard descriptions}
\format{
A data frame with 494 rows and 2 columns:
\itemize{
\item the types of beards;
\item the words used to describe them.
}
}
\source{
Applied mathematics department, Institut Agro Rennes-Angers
}
\usage{
beard
}
\description{
These data refer to 8 types of beards.
Each beard was evaluated by 62 assessors (except beard 8 which only had 60 evaluations).
}
\examples{
\dontrun{
# Processing time is often longer than ten seconds
# because the function uses a large language model.

data(beard)
beard[1:8,]
}
}
\keyword{datasets}
