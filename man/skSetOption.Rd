% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seekr-api.R
\name{skSetOption}
\alias{skSetOption}
\title{Sets seekr option according to the type.}
\usage{
skSetOption(type, id)
}
\arguments{
\item{type}{Components name (e.g. 'people', 'projects', ...).}

\item{id}{Character string with the identifier
of the component (id part).}
}
\value{
A list with the set option or NA if the type is not registered.
}
\description{
Sets seekr option according to the type.
}
\examples{
\dontrun{
skSetOption("people",111)
skSetOption("myid", 368)
skOptions()
skSetOption("bla",1) # can not be set
# Set option sk.aid
skSetOption("assay",1234)
skOptions()
# Delete/reset option sk.aid
skSetOption("assay",NULL)
skOptions()
Get options
skGetOption("myid")
skGetOption("inv")
}
}
\seealso{
\code{\link{skFindTitle}}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
