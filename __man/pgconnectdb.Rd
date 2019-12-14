\name{pgconnectdb}
\alias{pgconnectdb}
\title{Creation of an SQL database}

\description{
This function allows user to create a new SQL database and returns connexion informations. If this database already exists a simple connexion is made (no deletion of the existing database).
}

\usage{
pgconnectdb(driver = "PostgreSQL", host = "localhost", port = 5433, user, password, dbname)
}

\arguments{
  \item{driver}{the database management system (DBMS) name.}
  \item{host}{name (or IP address) of the machine hosting the database.}
  \item{port}{integer specifying the TCP port number that the PostgreSQL server is listening to}
  \item{user}{the user login to the DBMS.}
  \item{password}{the user password to the DBMS.}
  \item{dbname}{the database name to be created or accessed.}
}

\value{
Connexion informations in the format returned by the function \code{\link[DBI:dbConnect]{DBI}} of the package \code{\link[DBI:DBI]{DBI}}.
}

\author{Nicolas Casajus}

\note{
This function is generic and can be used in any other context to create a PostgreSQL database.
}

\seealso{
\code{\link{pgaddnewptt}}, \code{\link{pgupdatetb}}, \code{\link{pggetidbyptt}}, \code{\link{pgupdateinfos}}, \code{\link{pggetptts}}, \code{\link{pgextractlocs}}
}

\examples{
###
### See tutorial for a complete example of usage of this package.
}