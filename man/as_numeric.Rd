% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factors.R
\name{as_numeric}
\alias{as_numeric}
\alias{as_numeric.default}
\alias{as_numeric.factor}
\title{Converting factors to numeric}
\usage{
as_numeric(x)

\method{as_numeric}{default}(x)

\method{as_numeric}{factor}(x)
}
\arguments{
\item{x}{a vector}
}
\value{
a numeric vector of the same length as \code{x} in which each value
of \code{x} is converted to its actual numeric value
}
\description{
Converting factors to numeric
}
\details{
\code{as_numeric()} is motivated by the need to convert factors to their
numerical representation, rather than their numerically encoded levels.  See
the examples for differences between \code{base::as.numeric()} and
\code{as_numeric}.
}
\examples{
x <- factor(c(-1.9, 0.2, 3.5))
as.numeric(x)
as_numeric(x)

}
\seealso{
\code{as_numeric.factor} directly follows the suggestion from the 
\code{Warning} section of \code{\link{factor}}.
}
