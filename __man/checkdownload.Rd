\name{checkdownload}
\alias{checkdownload}

\title{Check content of a downloaded Argos file}

\description{
This function imports a raw Argos file, format fields and returns a list of PTT, the number of location by PTT and the spatial area of each PTT (estimated with a MCP95). One map is produced by PTT to visualize locations.
}

\usage{
checkdownload(file, dest)
}

\arguments{
  \item{file}{the name (relative or absolute) of the file to import.}
  \item{dest}{the folder to store outputs.}
}

\value{
This function return a data frame with 3 columns:
\item{PTT}{The identifier of the PTT}à
\item{Locations}{The number of locations}
\item{Area}{The estimated area of locations in km2}
This data frame and maps are saved on the disk.
}

\author{
Nicolas Casajus
}

\seealso{
\code{\link{readargos}}, \code{\link{stackfiles}}
}

\examples{
###
### See tutorial for a complete example of usage of this package.
}
