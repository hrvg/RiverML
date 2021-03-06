% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_benchmark.R
\name{get_outers}
\alias{get_outers}
\title{Get the outer folds for the nested resampling}
\usage{
get_outers(nu = 5, reps = 10, labels, tasks)
}
\arguments{
\item{nu}{\code{numeric}, number of folds for the \eqn{\nu}-fold cross-validation}

\item{reps}{\code{numeric}, number of repetitions for the repeated \eqn{\nu}-fold cross-validation}

\item{labels}{\code{character} or \code{factor} the labels}

\item{tasks}{list of \code{mlr} tasks}
}
\value{
\code{mlr} \code{resampleDesc}
}
\description{
Get the outer folds for the nested resampling
}
\keyword{ml-benchmark}
