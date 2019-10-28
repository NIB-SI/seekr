## ----c,echo=FALSE--------------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################
#

## ----d,echo=FALSE,results='hide'-----------------------------------------
options(width=70)
library(httr)


## ----fhIni---------------------------------------------------------------
#' Initialize FAIRDOMhub information
#'
#' Define FAIRDOMhub URL and user data
#'
#' @param test If TRUE, test server will be used..
#' @return A list with URL and user information. For side effect see Notes.
#' @notes The returned list is added to the options list under name 'fhub'.
#' @export
#' @keywords file
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' fhIni()
#' options("fhub")
#' #
#' fhIni(FALSE)
#' options("fhub")
#' }
fhIni <- function(test=TRUE){
mainurl <- "https://www.fairdomhub.org/"
testurl <- "https://testing.sysmo-db.org"
# My personal ids
my_main_id <- 808
my_test_id <- 368
# I will use the testing site for the development and experiments:
baseurl <- ifelse(test, testurl, mainurl)
myid <- ifelse(test, my_test_id, my_main_id)
usr <- ifelse(test, "ablejec", "ablejec")
pwd <- ifelse(test, "testni.1234", "Abink.9912")
tmp <- list(baseurl=baseurl, usr=usr, pwd=pwd, myid=myid)
   options(fhub=tmp)
invisible(tmp)
}


## ----fhGET---------------------------------------------------------------
#' Get inormation from repository.
#'
#' @param what Type of information (e.g. "person").
#' @param id Repository id of an item.
#' @param uri Repository base address (URI)..
#' @return File name (string).
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{get ...}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' options()$fhub$myid
#' r <- fhGET("people",options()$fhub$myid)
#' names(r)
#' r$status_code
#' }
fhGET <- function(what, id,
                   uri=options()$fhub$baseurl, ... ){
#                  uri="https://www.fairdomhub.org", ... ){
  if(!missing(what)) uri <- paste0(uri,"/",what)
  if(!missing(id)) uri <- paste0(uri,"/",id)
  r <- GET(uri,
         add_headers(Accept="application/json"))
  # cat("Status code:",r$status_code,"\n")
  invisible(r)
}


## ----fhData--------------------------------------------------------------
#' Get content from an *fh* object.
#'
#' @param r Object retrieved by fhGET.
#' @param part Name of the required element. If missing, a list with
#'     all relevant objects is returned.
#' @return File name (string).
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{get ...}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' options()$fhub$myid
#' r <- fhGET("people",options()$fhub$myid)
#' d <- fhData(r)
#' names(d)
#' d$last_name
#' fhData(r)$tools
#' # Get list of people
#' r <- fhGET("people")
#' d <- fhData(r)
#' length(d)
#' names(d)
#' names(d[[1]])
#' titles <- sapply(d,function(x) x$attributes$title)
#' head(titles)
#' # Get FAIRDOMhub user id
#' myname <- titles[1]
#' myname
#' d[[pmatch(myname,titles)]]
#' id <- d[[pmatch(myname,titles)]]$id
#' id
#' }
fhData <- function(r, part, ...){
  jsn <- "application/json"                
  if(missing(part)) invisible(content(r,"parsed",type=jsn)$data) else
  invisible(content(r,"parsed",type=jsn)$data[[part]])
}
fhDatas <- fhData


## ----fhFindId------------------------------------------------------------
#' Get details of component with id from an *fh* object.
#'
#' @param part Components name (e.g. 'people', 'projets', ...).
#' @param title Character string with the identifier
#'     of the component (Title part).
#' @return FAIRDOMhub component identifier: id, type and title.
#'     If argument title is missing, a data frame with identifiers
#'     for all items is returned.
#' @export
#' @keywords pisa
#' @seealso \code{\link{fhFindTitle}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' id <- fhFindId("people","Guest")
#' id
#' fhFindTitle("people",id)
#' # List of projects
#' projects <- fhFindId("projects")
#' head(projects)
#' }
fhFindId <- function(part, title){
     r <- fhGET(part)
     d <- fhData(r)
     titles <- t(sapply(d,function(x) c(id=x$id, type=x$type, title=x$attributes$title)))
     # Get FAIRDOMhub user id
     if(!missing(title)){
       id <- d[[pmatch(title,titles[,"title"])]]$id
       return(c(id=id,type=part, title=title))
     } else {
     return(titles)
     }
}


## ----fhFindTitle---------------------------------------------------------
#' Get details of component with id from an *fh* object.
#'
#' @param part Components name (e.g. 'people', 'projets', ...).
#' @param id Character string with the identifier
#'     of the component (id part).
#' @return FAIRDOMhub component identifier: id, type and title.
#'     If argument title is missing, a data frame with identifiers
#'     for all items is returned.
#' @export
#' @keywords pisa
#' @seealso \code{\link{fhFindTitle}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' id <- fhFindId("people","Guest")
#' id
#' idNew <- fhFindTitle("people", id[1])
#' idNew
#' all.equal(id, idNew)
#' }
fhFindTitle <- function(part, id){
     id <- as.vector(id[1])
     r <- fhGET(part, id )
     d <- fhData(r, "attributes")
     # Set FAIRDOMhub user title
     return(c(id=id, type=part, title=d$title))
     }

