\name{pgaddnewptt}
\alias{pgaddnewptt}
\title{Add new id/PTT informations in PostgreSQL tables}

\description{

}

\usage{
pgaddnewptt(dbname, noid, ptt, start)
}

\arguments{
  \item{dbname}{the connexion informations to the SQL database. Typically the output of \code{\link{pgconnectdb}}.}
  \item{noid}{the animal identifier to add or for which a new ptt has been add.}
  \item{ptt}{the ptt number.}
  \item{start}{the starting date of tracking.}
}

\details{
This function works with new/old fox and new/recycled ptt. It automatically creates a new pttuser (i.e. ******B) if ptt is recycled. Both pttinfos and foxnoids relations will be updated.
}

\value{
No return.
}

\author{Nicolas Casajus}

\note{
This operation can not be undone. For that, a confirmation is required before processing.
}

\seealso{
\code{\link{pgconnectdb}}, \code{\link{pgupdatetb}}, \code{\link{pggetidbyptt}}, \code{\link{pgupdateinfos}}, \code{\link{pggetptts}}, \code{\link{pgextractlocs}}
}

\examples{
###
### See tutorial for a complete example of usage of this package.
}