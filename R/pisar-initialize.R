## ----author, echo=FALSE--------------------------------------------------
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
#' @param x complete file path or  file.type name
#' @param ... any other arguments
#' @return File name (string)
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{fileType}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileName(".\\validation.Rnw")
#' fileName("./bla/validation.Rnw")
#' fileName("./validation.")
#' fileName("./validation")
fileName <- function(x,...){
gsub("\\..*$","",basename(x))
gsub("(.*)\\.(.*)","\\1",basename(x))
}


## ----fileType------------------------------------------------------------
#' Extract file type
#'
#' Extract file type from a file path.
#'
#' @param x complete file path or  file name
#' @param ... any other arguments
#' @return File type (string)
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileName(".\\validation.Rnw")
#' fileName("./bla/validation.Rnw")
#' fileName("./validation.")
#' fileName("./validation")
fileType <- function(x,...){
type <- gsub("(.*)\\.(.*)","\\2",basename(x))
if(type==x)  type  <- ""
return(type)
}
fileType("bla")
fileType("./bla.Rnw")


## ----fsummary------------------------------------------------------------
#' As factor summary of a data frame
#'
#' @param x data frame
#' @param ... any other arguments
#' @return summary object
#' @export
#' @note Argument ... not used
#' @keywords summary
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fsummary(data.frame(x=rnorm(20),txt=sample(letters,20,rep=TRUE)))
fsummary <- function(x,...){
    dims <- dimnames(x)
    x <- apply(x,2,factor)
    dimnames(x) <- dims
    summary(x)
}
fsummary(data.frame(x=rnorm(20),txt=sample(letters,20,rep=TRUE)))


## ----getRoot-------------------------------------------------------------
#' Get root directory for pISA layer
#'
#' @param x character characteristic for pISA layer
#' @param path path within the pISA-tree
#' @param ... any other arguments 
#' @return data frame with Key/value pairs
#' @export
#' @note Argument ... not used.
#' @keywords pISA
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' getRoot("p", path="d:/_p_prj/_I_inv/_S_st/_A_asy/other/doc")  
getRoot <- function(x="p",path=getwd(),...){
dirs <- strsplit(path,"/")[[1]]
nl <- length(dirs)-which(regexpr(paste0("_",x,"_"),dirs)>0)
if(length(nl)<=0) stop(paste("Path is not within a pISA-tree:\n"
        , path))
paste(rep("..",nl),collapse="/")
}
getRoot("A", path="d:/_p_prj/_I_inv/_S_st/_A_asy/other/doc")


## ----readMeta------------------------------------------------------------
#' Read metadata file from the given directory
#'
#' @param x file path to the pISA layer
#' @param ... any other arguments
#' @return data frame with Key/value pairs with class 'pISAmeta'
#' @export
#' @note Metadata table gets the class 'Dlist' to inherit a convenient print.
#' @keywords pISA
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' .pISAloc <- system.file("extdata","_p_Demo",package="pisar")
#' readMeta(.pISAloc)
#' }
readMeta <- function(x=".",  ...){
d <- tolower(dir(x))
d
lfn <- d[regexpr(".*_metadata",d)>0]
if(length(lfn)==0) warning("No metadata file found")
if(length(lfn)>1) warning("More tha one metadata file found:", d)
if(length(lfn)==1){
  p <- read.table(file.path(x, lfn)
  ,sep="\t", stringsAsFactors=FALSE, col.names=c("Key","Value"))
class(p)<- c("Dlist", "pISAmeta", class(p))
} else {p = ""}
return(p)
}
##
.pISAloc <- "../inst/extdata/_p_Demo"
readMeta(.pISAloc)


## ----print.pISAmeta------------------------------------------------------
#' Print metadata object as Dlist
#'
#' @param x metadata object, data.frame with two columns
#' @param ... any other arguments
#' @export
#' @note Metadata table is printed in convenient Dlist form.
#' @keywords package
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' print(data.frame(Key=c("First","Second")
#' , Value=c("Description 1", "Description 2"))
print.pISAmeta <- function(x,  ...){
    #if(inherits(x,"pISAmeta")
    print.Dlist(x)
    }
readMeta(.pISAloc)

