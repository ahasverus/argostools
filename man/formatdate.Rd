\name{formatdate}
\alias{formatdate}
\alias{formatcoords}
\title{Formating date and coordinates fields in Argos locations}

\description{
These two functions homogenize the date, longitude and latitude columns in raw Argos data. 
}

\usage{
formatdate(data, dte = "dateloc")
formatcoords(data, lat = "latitude", lon = "longitude")
}

\arguments{
  \item{data}{a data frame with the Argos locations.}
  \item{dte}{the name of the column of the date of location.}
  \item{lat}{the name of the column of the latitude.}
  \item{lon}{the name of the column of the longitude.}
}

\details{
The date of location is transformed in the \code{\link[base:as.POSIXct]{POSIXct}} format. The two coordinates fields can be in the degrees-minutes-seconds or decimal degrees format and are converted in the decimal degrees format.
}

\author{Nicolas Casajus}

\note{
These functions are called by the function \code{\link{readargos}} but can also be used externally.
}

\seealso{
\code{\link{rmduplicates}}, \code{\link{deletenas}}, \code{\link{readargos}}, \code{\link{stackfiles}}
}

\examples{
###
### See tutorial for a complete example of usage of this package.
}
