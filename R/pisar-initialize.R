## ----author, echo=FALSE--------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################




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
#' @note The path should be compliant with the pISA-tree structure.
#'     Path defaults to the working directory, which is
#'     usually in or below an assay. Argument ... is not used.
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
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .aroot <- getRoot("A")
#' .ameta <- readMeta(.aroot)
#' .ameta
#' setwd(oldwd)
#'
readMeta <- function(x=".",  ...){
d <- tolower(dir(x))
d
lfn <- d[regexpr(".*_metadata",d)>0]
if(length(lfn)==0) warning("No metadata file found")
if(length(lfn)>1) warning("More tha one metadata file found:", d)
if(length(lfn)==1){
  p <- rio::import(file.path(x, lfn)
  ,sep="\t", stringsAsFactors=FALSE, col.names=c("Key","Value"))
class(p)<- c("Dlist","pISAmeta",  class(p))
} else {p = ""}
return(p)
}
##
.pISAloc <- system.file("extdata","_p_Demo",package="pisar")
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
#' .pISAloc <- system.file("extdata","_p_Demo",package="pisar")
#' readMeta(.pISAloc)
print.pISAmeta <- function(x,  ...){
    #if(inherits(x,"pISAmeta")
    print.Dlist(x)
    }



## ----getMeta-------------------------------------------------------------
#' Get metadata value
#'
#' @param x two column character data frame with Key / Value pairs
#' @param item string, item name
#' @param nl logical, expand backslash character for new lines
#' @param ... any other arguments (not used at the moment)
#' @return character string with key value
#' @export
#' @note Parameter item is matched exactly to the item names.
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .iroot <- getRoot("I")
#' .idesc <- readMeta(.iroot)
#' getMeta(.idesc, "Description")
#' setwd(oldwd)
getMeta <- function(x,item,nl=TRUE){
item <- paste0(gsub(":","",item),":")
ret <- unclass(x[match(item, x[,1]), 2])
if(is.character(ret)&&nl) ret <- sub("\\\\n","\n",ret)
return(ret)
}






## ----getLayer------------------------------------------------------------
#' Get pISA layer name
#'
#' @param x layer character (one of p, I, S, or A)
#' @param path, deafaults to working directory
#' @return character string with layer name
#' @export
#' @note 
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .pname <- getLayer("p")
#' .pname
#' getLayer("I")
#' getLayer("S")
#' getLayer("A")
getLayer <- function(x, path=getwd()){
  loc <- strsplit(path,"/")[[1]]
  lyr <- paste0("_",x,"_")
  lname <- loc[regexpr(lyr,loc)==1]
  if(length(lname)==0) {
    lname <- ""
    warning("No layer '", x, "' in path")
    }
  return(lname)
  }

