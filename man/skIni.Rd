% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seekr-api.R
\name{skIni}
\alias{skIni}
\title{Initialize FAIRDOMhub information}
\usage{
skIni(
  url = "https://www.fairdomhub.org/",
  usr = NULL,
  pwd = NULL,
  myid = NULL,
  prid = NULL,
  pid = NULL,
  iid = NULL,
  sid = NULL,
  aid = NULL
)
}
\arguments{
\item{url}{SEEK based server address. It can also be a list with
four components interpreted as url, usr, pwd and myid.}

\item{usr}{user name.}

\item{pwd}{password.}

\item{myid}{numeric user id.}

\item{prid}{programme number.}

\item{pid}{project number.}

\item{iid}{investigation number.}

\item{sid}{study number.}

\item{aid}{assay number.}
}
\value{
A list with URL and user information. For side effect see Notes.
}
\description{
Define FAIRDOMhub url and user data
}
\note{
The returned list is added to the
     \code{options()} list under name 'seekr'.

At the moment two locations are available: main FAIRDOMHub 
      (https://www.fairdomhub.org/) and testing site
      (https://testing.sysmo-db.org).
}
\examples{
\dontrun{
skIni(url="https://www.fairdomhub.org/", usr="username"
   , pwd="secret", myid=888)
options("seekr")
#
fh <- list(url="https://www.fairdomhub.org/", usr="username"
   , pwd="secret", myid=888)
skIni(fh)
options("seekr")
}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\keyword{file}
