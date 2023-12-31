% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymlnorm.R
\name{mymlnorm}
\alias{mymlnorm}
\title{mymlnorm}
\usage{
mymlnorm(x, mu, sig, ...)
}
\arguments{
\item{x}{data}

\item{mu}{mean}

\item{sig}{standard deviation}

\item{...}{etc}
}
\value{
a graph of the maximum likelihood estimates for population mean and standard deviation
}
\description{
mymlnorm
}
\examples{
mymlnorm(c(15,14,13,12),mu=seq(10,14,length=1000),sig=seq(.1,4,length=1000))
}
