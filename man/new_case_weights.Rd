% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case-weights.R
\name{new_case_weights}
\alias{new_case_weights}
\title{Extend case weights}
\usage{
new_case_weights(x, ..., class)
}
\arguments{
\item{x}{An integer or double vector.}

\item{...}{Name-value pairs defining attributes}

\item{class}{Name of subclass.}
}
\value{
A new subclassed case weights vector.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{new_case_weights()} is a developer oriented function for constructing a new
case weights type. The \verb{<case_weights>} type itself is an \emph{abstract} type
with very little functionality. Because of this, \code{class} is a required
argument.
}
\examples{
new_case_weights(1:5, class = "my_weights")
}
