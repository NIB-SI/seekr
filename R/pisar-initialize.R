## ----a, echo=FALSE-------------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################


## ----testing,echo=FALSE--------------------------------------------------
.testing <- FALSE


## ----Package description-------------------------------------------------
#' pisar: pISA-tree support functions
#'
#' The package provides several functions for support
#' and use of pISA-tree.
#'
#' @section Usage:
#'
#' pISA-tree is a standardized directory tree
#' for storing projet information under ISA paradigm.
#' The set of functions have two fold purpose:
#'     1. To enable use of metadata for reproducible documents
#'     2. To enable automated upload to external repository
#' (FAIRDOMhub).
#'
#'
#' @docType package
#' @name pisar
NULL


## ----fileName------------------------------------------------------------
#' Extract file name
#'
#' Extract file name from a file path.
#'
#' @param x file path and file name
#' @param ... any other arguments (not treated at the moment
#' @return File name (string)
#' @export
#' @note
#' @references
#' @keywords file
#' @title
#' @seealso \code{\link{fileType}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileName(".\\validation.Rnw")
fileName <- function(x,...){
gsub("\\..*$","",basename(x))
gsub("(.*)\\.(.*)","\\1",basename(x))
}



