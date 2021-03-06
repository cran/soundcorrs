% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{path2name}
\alias{path2name}
\title{Extract file name from path string.}
\usage{
path2name(...)
}
\arguments{
\item{x}{[character] Path to a file.}
}
\value{
[character] The name of the file.
}
\description{
Convert a path to file to just its name, without the path or the extension.
}
\details{
A combination of \code{\link{basename}} to strip the path, and \code{tools::file_path_sans_ext} to strip the extension.
}
\examples{
sapply (list.files("~",full.names=TRUE), soundcorrs:::path2name)
}
\keyword{internal}
