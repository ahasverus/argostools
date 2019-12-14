\name{deletenas}
\alias{deletenas}
\title{Remove NA values in Argos locations}

\description{
This function deletes locations where no ptt, no coordinates or no date of location are reported.
}

\usage{
deletenas(data, lat = "latitude", lon = "longitude", dte = "dateloc", ptt = "platform")
}

\arguments{
  \item{data}{a data frame with the Argos locations.}
  \item{lat}{the name of the column of the latitude.}
  \item{lon}{the name of the column of the longitude.}
  \item{dte}{the name of the column of the date of location.}
  \item{ptt}{the name of the column of the ptt.}
}

\author{Nicolas Casajus}

\note{
This function is called by the function \code{\link{stackfiles}} but can also be used externally.
}

\seealso{
\code{\link{rmduplicates}}, \code{\link{formatdate}}, \code{\link{formatcoords}}, \code{\link{stackfiles}}
}
\examples{
###
### See tutorial for a complete example of usage of this package.
}
