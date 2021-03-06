% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{checkCount}
\alias{checkCount}
\title{Check if the \code{count} argument is correct.}
\usage{
checkCount(count)
}
\arguments{
\item{count}{[character] The string to check.}
}
\value{
[character] Either \code{"a"} or \code{"r"}.
}
\description{
Makes sure the \code{count} argument has one of the many available values.
}
\details{
Functions which produce contingency tables, \code{\link{coocc}} and \code{\link{summary}}, have an argument \code{count} which can take as many as six different values which all point to just two unique behaviours. \code{checkCount} reduces this multitude to just the two meaningful values. It also throws an error if \code{count} is none of the six possibilities.
}
\examples{
soundcorrs:::checkCount ("abs")
soundcorrs:::checkCount ("absolute")
}
\seealso{
\code{\link{checkUnit}}
}
\keyword{internal}
