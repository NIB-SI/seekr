% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seekr-api.R
\name{skSkeleton}
\alias{skSkeleton}
\title{Create *sk* skeleton.}
\usage{
skSkeleton(type = "assay")
}
\arguments{
\item{type}{Component type (e.g. 'people', 'projects', ...).
a list with minimal information (Title, Description, *ToDo: add fields*).}
}
\value{
A list with the minimal information structure.
}
\description{
Creates *sk* object with required structure.
}
\examples{
\dontrun{
require(jsonlite)
type <- "projects"
sp <- skSkeleton( type = type )
str(sp)
type = "investigations"
si <- skSkeleton( type = type )
str(si)
type = "studies"
ss <- skSkeleton( type = type )
str(ss)
type = "assays"
sa <- skSkeleton( type = type )
str(sa)
type = "data_files"
file =
sdata <- skSkeleton( type = type )
str(sdata)
}

}
\seealso{
\code{\link{skCreate}}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\keyword{pisa}
