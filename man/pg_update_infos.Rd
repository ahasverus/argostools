% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pg_update_infos.R
\name{pg_update_infos}
\alias{pg_update_infos}
\title{Update values on a PostgreSQL table}
\usage{
pg_update_infos(dbname, tbname, field, value, key, id)
}
\arguments{
\item{dbname}{A DBI connexion information}

\item{tbname}{PostgreSQL table name to be updated}

\item{field}{Field to be updated}

\item{value}{New value of the field}

\item{key}{Key of the condition}

\item{id}{Value  of the condition}
}
\description{
Update values on a PostgreSQL table.
}
\examples{
## See vignette

}
