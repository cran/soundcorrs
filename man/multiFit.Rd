% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{multiFit}
\alias{multiFit}
\title{Fit multiple models to one dataset.}
\usage{
multiFit(models, data, fun = nls, ...)
}
\arguments{
\item{models}{[list] A list of models to fit \code{data} to. Each element must be a list with at least two named fields: \code{formula} which contains the formula, and \code{start} which is a list of lists of starting estimates.}

\item{data}{[numeric data.frame/list] A list of vectors to fit \code{models} to.}

\item{fun}{[function] The function to use for fitting. Defaults to \code{nls}.}

\item{...}{Additional arguments passed to \code{fun}.}
}
\value{
[list.multiFit] A list of results returned by \code{fun} or, if it ended with an error, \code{NULL}.
}
\description{
Apply a fitting function, with multiple models and multiple starting estimates, to one dataset.
}
\details{
Finding the right model and the right starting estimates for a model is often a time consuming process, very inconvenient to do manually. This function automates it as much as possible. It takes a list of models and starting estimates, and fits them to data not stopping whenever an error occurs or a warning is issued. Error and warning messages are saved and can be inspected in the output, they just do not halt the process. \code{multiFit} has an extension in the form of \code{\link{fitTable}} which applies multiple models to multiple datasets.
}
\examples{
set.seed (27)
dataset <- data.frame (X=1:10, Y=(1:10)^2+runif(10,-10,10))
models <- list (
"model A" = list (
	formula = "Y ~ X^a",
	start = list (list(a=100), list(a=1))),
"model B" = list (
	formula = "Y ~ a*(X+b)",
	start = list (list(a=1,b=1)))
)
multiFit (models, dataset)
}
\seealso{
\code{\link{fitTable}}, \code{\link{summary.list.multiFit}}
}
