% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seekr-api.R
\name{skCheckAndCreate}
\alias{skCheckAndCreate}
\title{Check if layer exists and create it if not.}
\usage{
skCheckAndCreate(
  type,
  layers,
  root = skGetOption("root"),
  create = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  na.access = "no_access"
)
}
\arguments{
\item{type}{character. ISA layer type (words starting with
I, S, or A, can be lowercase)}

\item{layers}{character vector. Info about layers,
usually the result of a call to skSetLayers.}

\item{root}{character. Root used to create the file list,
(usually?) pointing to the investigation above the working directory.}

\item{create}{logical. Create layer if not existing?}

\item{verbose}{logical. Print intermediate results?}

\item{quiet}{logical. Print result?}
}
\description{
Check if layer exists and create it if not.
}
\examples{
\dontrun{
astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
oldwd <- setwd(system.file("extdata",astring,package="seekr"))
oldwd
require(pisar)
require(httr)
require(jsonlite)
pisa <- pisa()
skReset()
options(.sk$test)
seekignore <- readLines(file.path(.iroot, "seekignore.txt"))
seekignore
dirs <- skFilesToUpload(.iroot)
layers <- skSetLayers(dirs[7], root=.iroot)
skFindId("projects", layers$pname)
(layers$iname <- paste(layers$iname, date()))
layers
print.simple.list(skOptions())
system.time(skCheckAndCreate("I",layers, create=TRUE, verbose=TRUE))
print.simple.list(skOptions())
system.time(skCheckAndCreate("S",layers, create=TRUE, verbose=TRUE))
print.simple.list(skOptions())
system.time(skCheckAndCreate("A",layers, create=TRUE, verbose=TRUE))
print.simple.list(skOptions())
if(interactive()) setwd(oldwd)
getwd()
}
}
\author{
Andrej Blejec \email{andrej.blejec@nib.si}
}
\keyword{pisa,}
\keyword{seekr}
