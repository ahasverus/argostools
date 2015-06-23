\name{formatinfos}
\alias{formatinfos}
\title{Formating PTT and animal informations}

\description{
This function standardizes informations (starting and ending dates) about ptt and animal tracking.
}

\usage{
formatinfos(data, noid = "noid", ptt = "platform", start = "start", end = "end")
}

\arguments{
  \item{data}{a data frame with animal and ptt informations. See Details section for further information.}
  \item{noid}{the name of the column of the animal identifier.}
  \item{ptt}{the name of the column of the ptt.}
  \item{start}{the date of start tracking of the ptt.}
  \item{end}{the date of end tracking of the ptt. Can be unspecified if the tracking is still ongoing.}
}
\details{
One animal can be tracked by several successive ptts and one ptt can be used on different animals. So it is important to document the start and end dates of tracking of one animal by one ptt. The function \code{formatinfos} standardizes these informations in order to create non-redundant tables that will be used by \code{pg-*} functions.}

\value{
A list with two data frame:
  \item{tabIds}{a data frame with the animal identification and all the ptt used on it.}
  \item{tabPtt}{a data frame with the start and the end of track of each ptt.}
}

\author{Nicolas Casajus}

\note{
Author recommands using this function in the first place to standardize ptt and animal informations. Furthermore, when one ptt was used on different animal, it is important to distinguish these differents situations by adding a letter to ptt. For instance, when the ptt 123456 was used on two animals, it can be renamed in 123456A for the first animal and in 123456B for the second.}

\seealso{
\code{\link{addid}}
}

\examples{
###
### See tutorial for a complete example of usage of this package.
}