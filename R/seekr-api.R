## ----c,echo=FALSE---------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################
#

## ----d,echo=FALSE,results='hide'------------------------------------
options(width=70)
library(httr)


## ----skIni----------------------------------------------------------
#' Initialize FAIRDOMhub information
#'
#' Define FAIRDOMhub url and user data
#'
#' @param url SEEK based server address. It can also be a list with
#'   four components interpreted as url, usr, pwd and myid.
#' @param usr user name.
#' @param pwd password.
#' @param myid numeric user id.
#' @param prid programme number.
#' @param pid project number.
#' @param iid investigation number.
#' @param sid study number.
#' @param aid assay number.
#' @return A list with URL and user information. For side effect see Notes.
#' @note The returned list is added to the
#'      \code{options()} list under name 'seekr'. 
#' @note At the moment two locations are available: main FAIRDOMHub 
#'       (https://www.fairdomhub.org/) and testing site
#'       (https://testing.sysmo-db.org).
#' @export
#' @keywords file
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni(url="https://www.fairdomhub.org/", usr="username"
#'    , pwd="secret", myid=888)
#' options("seekr")
#' #
#' fh <- list(url="https://www.fairdomhub.org/", usr="username"
#'    , pwd="secret", myid=888)
#' skIni(fh)
#' options("seekr")
#' }
skIni <- function(url="https://www.fairdomhub.org/"
    , usr = NULL
    , pwd = NULL
    , myid = NULL
    , prid = NULL
    , pid = NULL
    , iid = NULL
    , sid = NULL
    , aid = NULL
    ){
    if(is.list(url)) {
      tmp <- url
      url <- tmp[1]
      usr <- tmp[2]
      pwd <- tmp[3]
      myid <- tmp[4]
      }
#     
tmp <- list(baseurl = url
   , usr = usr
   , pwd = pwd
   , myid = as.character(myid)
   , prid = as.character(prid)
   , pid = as.character(pid)
   , iid = as.character(iid)
   , sid = as.character(sid)
   , aid = as.character(aid)
   )
   options(seekr = tmp)
invisible(tmp)
}


## ----skParse--------------------------------------------------------
#' Parse the response from SEEK API
#'
#' @param resp Response from SEEK API.
#' @return An object (list) of class \code{seek_api}.
#' @export
#' @seealso \code{\link{skGet}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' options()$seekr$myid
#' r <- skGet("people",options()$seekr$myid)
#' names(r)
#' r$response$status_code
#' status_code(r$response)
#' names(r$content)
#' r
#' r <- skGet("people")
#' length(r$content)
#' names(r$content)
#' r$content[[1]]
#' names(r)
#' r
#' }
skParse <- function(resp, ...){
    jsn <- "application/json"
    parsed <- invisible(content(resp,"parsed",type=jsn)$data)
    if(class(parsed$content) %in% "raw") parsed$content <- rawToChar(parsed$content)
     if (http_error(resp)) {
     stop(
      sprintf(
        "SEEK API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        resp$header$status,
        "https://app.swaggerhub.com/apis/FAIRDOM/SEEK/0.1"
      ),
      call. = FALSE
    )
  }
  if(length(parsed$meta$base_url)!=1) url <- "" else
  url <- httr::modify_url(parsed$meta$base_url,path=parsed$links)
  ret <-    structure(
       list(
          id = parsed$id
        , path = parsed$links$self
        , url = url
        , status = resp$status_code
        , title = parsed$content$attributes$title
        , content = parsed
        , response = resp
       ),
       class = "seek_api"
       )
  return(ret)

}


## ----print.seek_api-------------------------------------------------
#' Print method for seek_api object
#'
#' @param x Object of class \code{seek_api}.
#' @param content If FALSE (default), content is no printed.
#' @return An object (list) of class \code{seek_api}.
#' @export
#' @seealso \code{\link{skParse}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' options()$seekr$myid
#' r <- skGet("people",options()$seekr$myid)
#' # Print contents
#' print( r, TRUE)
#' # Short version, default
#' r
#' r$response$status_code
#' status_code(r$response)
#' }
#'
print.seek_api <- function(x, content=FALSE){
cat("Status:", x$response$headers$status, "\n")
#if(x$response$status==200)
{
pr <- x$content
cat("Object:", x$url,"\n")
cat("Path  :", x$path,"\n")
cat("Title :", x$content$attributes$title, "\n")
if(content) {
cat(rep("-",20),"\n\n")
 str(x$content)
 }
invisible(x)
}
}


## ----skGet----------------------------------------------------------
#' Get inormation from repository.
#'
#' @param type Type of information (e.g. "person").
#' @param id Repository id of an item.
#' @param uri Repository base address (URI)..
#' @return An object (list) of class \code{seek_api}.
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{get}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' options()$seekr$myid
#' r <- skGet("people",options()$seekr$myid)
#' names(r)
#' r$response$status_code
#' status_code(r$response)
#' r
#' # Non existent user
#' skGet("people",0)
#' }
skGet <- function(type, id,
                   uri=options()$seekr$baseurl, ... ){
#                  uri="https://www.fairdomhub.org", ... ){
  if(!missing(type)) uri <- paste0(uri,"/",type)
  if(!missing(id)) uri <- paste0(uri,"/",id)
  ua <- httr::user_agent("https://github.com/nib-si/seekr")
  skLog("skGet", uri)
  fht <- system.time(
  resp <- httr::GET(uri,
         add_headers(Accept="application/json")
         , ua
         )
  )
  cat("Status code:",resp$status_code,"\n")

  if( resp$status_code < 300) {
  parsed <- skParse(resp)
  skLog( resp$headers$status, round(fht["elapsed"],2), parsed$url)
  } else {
  parsed <- resp$status_code
  skLog( resp$headers$status, round(fht["elapsed"],2))
  }
  return(parsed)
}
#



## ----skData---------------------------------------------------------
#' Get content from an *sk* object.
#'
#' @param r Object retrieved by skGet.
#' @param type Name of the required element. If missing, a list with
#'     all relevant objects is returned.
#' @return File name (string).
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{get}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' options()$seekr$myid
#' r <- skGet("people",options()$seekr$myid)
#' d <- skData(r,"attributes")
#' names(d)
#' d$last_name
#' skData(r)$tools
#' # Get list of people
#' r <- skGet("people")
#' d <- skData(r)
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
## skData <- function(r, node, ...){
##   if(class(r)=="seek_api") r <- r$response
##   jsn <- "application/json"
##   if(missing(node)) invisible(content(r,"parsed",type=jsn)$data) else
##   invisible(content(r,"parsed",type=jsn)$data[[node]])
## }
skData <- function(r, node, ...){
  if(class(r)=="seek_api") r <- r$content
  jsn <- "application/json"
  if(missing(node)) invisible(r) else
  invisible(r[[node]])
}
# skDatas <- skData


## ----skFindId-------------------------------------------------------
#' Get details of component with id from an *sk* object.
#'
#' @param type Components name (e.g. 'people', 'projects', ...).
#' @param title Character string with the identifier
#'     of the component (title part).
#' @return FAIRDOMhub component identifier: id, type and title.
#'     If argument title is missing, a data frame with identifiers
#'     for all items is returned.
#' @note If item is not found, value 0 is returned as id.
#' @export
#' @keywords pisa
#' @seealso \code{\link{skFindTitle}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' id <- skFindId("people","Guest")
#' id
#' skFindTitle("people",id)
#' # does not exist
#' skFindId("people","No User")
#' # List of projects
#' projects <- skFindId("projects")
#' head(projects)
#' }
skFindId <- function(type, title){
     r <- skGet(type)
     d <- skData(r)
     titles <- t(sapply(d,function(x) c(id=x$id, type=x$type, title=x$attributes$title)))
     # Get FAIRDOMhub user id
     if(!missing(title)){
       id <- d[[pmatch(title,titles[,"title"])]]$id
       if(is.null(id)) id <- 0
       return(c(id=id,type=type, title=title))
     } else {
     return(titles)
     }
}


## ----skFindTitle----------------------------------------------------
#' Get details of component with id from an *sk* object.
#'
#' @param type Components name (e.g. 'people', 'projects', ...).
#' @param id Character string with the identifier
#'     of the component (id part).
#' @return FAIRDOMhub component identifier: id, type and title.
#'     If argument title is missing, a data frame with identifiers
#'     for all items is returned. See note.
#' @note If item is not found, empty string is returned as title.
#' @export
#' @keywords pisa
#' @seealso \code{\link{skFindTitle}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' skIni()
#' id <- skFindId("people","Guest")
#' id
#' idNew <- skFindTitle("people", id[1])
#' idNew
#' all.equal(id, idNew)
#' skFindTitle("people",0)
#' }
skFindTitle <- function(type, id){
     id <- as.vector(id[1])
     r <- skGet(type, id )
     if( class(r)=="integer" && r > 300) {
     title <- ""
     } else {
     d <- skData(r, "attributes")
     title <- d$title
     # Set FAIRDOMhub user title
     }
     return(c(id=id, type=type, title=title))
     }


## ----skSkeleton-----------------------------------------------------
#' Create *sk* skeleton.
#'
#' Creates *sk* object with required structure.
#'
#' @param type Component name (e.g. 'people', 'projets', ...).
#' @param meta Data frame with pISA metadata or
#' a list with minimal information (Title, Description, *ToDo: add fields*).
#' @return A list with the minimal information structure.
#' @export
#' @keywords pisa
#' @seealso \code{\link{skCreate}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' require(jsonlite)
#' meta= list(Title = "Test layer", Description = "Some description")
#' type = "projects"
#' sp <- skSkeleton( type = type
#'   , meta= meta
#'   )
#' str(sp)
#' type = "investigations"
#' si <- skSkeleton( type = type
#'   , meta= meta
#'   )
#' str(si)
#' type = "studies"
#' ss <- skSkeleton( type = type
#'   , meta= meta
#'   )
#' str(ss)
#' type = "assays"
#' sa <- skSkeleton( type = type
#'   , meta= meta
#'   )
#' str(sa)
#'
#' type = "data_files"
#' file =
#' sdata <- skSkeleton( type = type
#'   , meta= meta
#'   )
#' str(sdata)
#' }
#'
skSkeleton <- function (type = "assay", meta, file){
sj <- switch( type
, projects =
'{
  "data": {
    "type": "projects",
    "attributes": {
#      "avatar": null,
      "title": "*",
      "description": "*",
      "web_page": "",
      "wiki_page": "",
      "default_license": "CC-BY-4.0",
      "default_policy": {
        "access": "download",
        "permissions": [
                    {
                        "resource": {
                            "id": "*",
                            "type": "people"
                        },
                        "access": "manage"
                    }
#          ,{
#            "resource": {
#              "id": "*",
#              "type": "projects"
#            },
#            "access": "manage"
#          }
#         ,{
#            "resource": {
#              "id": "",
#              "type": "institutions"
#            },
#            "access": "view"
#          }
        ]
      }
#      , "members": {
#            "person_id": "*",
#            "institution_id": "*"
#      }
    },
    "relationships": {
      "programmes": {
        "data": [
          {
            "id": "*",
            "type": "programmes"
          }
        ]
      }
      , "creators": {
                   "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      }
      , "project_administrators": {
                   "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      },
      "asset_housekeeper": {
                "data": [
                    {
                        "id": "*",
                        "type": "people"
                    }
                ]
            }

#     , "organisms": {
#        "data": [
#          {
#            "id": "*",
#            "type": "organisms"
#          }
#        ]
#      }
    }
  }
}'
, investigations =
'{
  "data": {
    "type": "investigations",
    "attributes": {
      "title": "*",
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "*",
              "type": "projects"
            },
            "access": "manage"
          }
        ]
      },
      "description": "*",
      "other_creators": ""
    },
    "relationships": {
      "projects": {
        "data": [
          {
            "id": "*",
            "type": "projects"
          }
        ]
      },
#      "publications": {
#        "data": [
#          {
#            "id": "",
#            "type": "publications"
#          }
#        ]
#      },
      "creators": {
        "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      }
    }
  }
}'
, studies =
'{
  "data": {
    "type": "studies",
    "attributes": {
      "title": "*",
      "description": "*",
      "experimentalists": "",
      "person_responsible_id": "*",
      "other_creators": "",
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "*",
              "type": "projects"
            },
            "access": "manage"
          }
        ]
      }
    },
    "relationships": {
      "investigation": {
        "data": {
          "id": "*",
          "type": "investigations"
        }
      }
##       , "publications": {
##         "data": [
##           {
##             "id": "",
##             "type": "publications"
##           }
##         ]
##       }
      , "creators": {
        "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      }
    }
  }
}'
, assays =
'{
  "data": {
    "type": "assays",
    "attributes": {
      "title": "*",
      "assay_class": {
        "title": "",
        "key": "EXP",
        "description": ""
      },
      "assay_type": {
        "label": "",
        "key": ""
##      , "uri": ""
      }
##      , "technology_type": {
##        "label": "*",
##        "uri": ""
##      }
      , "other_creators": "",
      "description": "*",
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "*",
              "type": "projects"
            },
            "access": "manage"
          }
        ]
      }
    },
    "relationships": {
      "study": {
        "data": {
          "id": "*",
          "type": "studies"
        }
      }
##       , "publications": {
##         "data": [
##           {
##             "id": "16",
##             "type": "publications"
##           }
##         ]
##       }
##       , "organisms": {
##         "data": [
##           {
##             "id": "3",
##             "type": "organisms"
##           }
##         ]
##       },
##       "sops": {
##         "data": [
##           {
##             "id": "4",
##             "type": "sops"
##           }
##         ]
##       },
##       "models": {
##         "data": [
##           {
##             "id": "5",
##             "type": "models"
##           }
##         ]
##       },
##       "data_files": {
##         "data": [
##           {
##             "id": "16",
##             "type": "data_files"
##           }
##         ]
##       },
##       "documents": {
##         "data": [
##           {
##             "id": "32",
##             "type": "documents"
##           }
##         ]
##       }
      , "creators": {
        "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      }
    }
  }
}'
, documents =
'{
  "data": {
    "type": "documents",
    "attributes": {
      "title": "A Maximal Document",
      "description": "This is the description",
      "tags": [
        "tag1",
        "tag2"
      ],
      "license": "CC-BY-4.0",
      "other_creators": "John Smith, Jane Smith",
      "content_blobs": [
        {
          "original_filename": "a_pdf_file.pdf",
          "content_type": "application/pdf"
        }
      ],
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "359",
              "type": "projects"
            },
            "access": "edit"
          }
        ]
      }
    },
    "relationships": {
      "creators": {
        "data": [
          {
            "id": "234",
            "type": "people"
          }
        ]
      },
      "projects": {
        "data": [
          {
            "id": "359",
            "type": "projects"
          }
        ]
      },
      "assays": {
        "data": [
          {
            "id": "38",
            "type": "assays"
          }
        ]
      }
    }
  }
}'
, data_files =
'{
  "data": {
    "type": "data_files",
    "attributes": {
      "title": "A Maximal Data File",
      "description": "This is the description",
      "tags": [
        "tag1",
        "tag2"
      ],
      "license": "CC-BY-4.0",
      "other_creators": "",
      "content_blobs": [
        {
          "original_filename": "*",
          "content_type": "*"
        }
      ],
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "*",
              "type": "projects"
            },
            "access": "manage"
          }
        ]
      }
    },
    "relationships": {
      "creators": {
        "data": [
          {
            "id": "*",
            "type": "people"
          }
        ]
      },
      "projects": {
        "data": [
          {
            "id": "*",
            "type": "projects"
          }
        ]
      },
      "assays": {
        "data": [
          {
            "id": "*",
            "type": "assays"
          }
        ]
      }
    }
  }
}'
, '{"Error":"No such type"}'
)
# keep uncommented lines
sx <- unlist(strsplit(sj,"\n"))
sx <- sx[!grepl("^#",sx)]
sj <- paste(sx,collapse="\n")
sr <- jsonlite::fromJSON(sj, simplifyVector = TRUE)
return(sr)
}


## ----skLog----------------------------------------------------------
#' Writes a note to a log file.
#'
#' @param ... Objects to form a line.
#' @param file Log file name.
#' @param append Control append/rewrite mode.
#' @return System time of invoking..
#' @export
#' @keywords pisa
#' @seealso \code{\link{skGet}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' tst <- function(){
#'   skLog("Test", "writing to logfile", file="")
#'   fht <- system.time(Sys.sleep(1))
#'   skLog( "Time:", round(fht["elapsed"],2))

#' }
#' tst()
#' rm(tst)
#'
#' @rdname skLog
#' @export skLog
skLog <- function( ..., file="FAIRDOM.log",append=TRUE){
   cat(paste(Sys.time(),..., "\n"), file=file, append=append)
   }


## -------------------------------------------------------------------
#' Determine MIME type for file.
#'
#' @param File name.
#' @return MIME type string.
#' @export
#' @keywords pisa
#' @seealso
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' contentType("bla.txt")
#' contentType("bla.pdf")
#' contentType("bla.tar.gz")
#' contentType("bla.bla")
#'
contentType <- function(x){
  switch(tolower(fileType(x))
         , txt  = "text/plain"
         , rnw  = "text/plain"
         , log  = "text/plain"
         , rmd  = "text/markdown"
         , md   = "text/markdown"
         , csv  = "text/plain"
         , tsv  = "text/plain"
         , bat  = "text/plain"
         , htm 	= "text/html"
         , html = "text/html"
         , pdf  = "application/pdf"
         , doc  = "application/msword"
         , docx = "application/msword"
         , xls  = "application/excel"
         , xlsx = "application/excel"
         , tex  = "text/plain"
         , gz   = "application/x-gz"
         , tar  = "application/x-tar"
         , jpg  = "image/jpeg"
         , jpeg = "image/jpeg"
         , png  = "image/png"
         , tif  = "image/tiff"
         , tiff = "image/tiff"
         , "application/octet-stream"
         )
}


## ----skCreate-------------------------------------------------------
#' Create pISA layer or *sk* component.
#'
#' @param type Component name (e.g. 'people', 'projets', ...).
#' @param meta Data frame with pISA metadata or
#'     a list with minimal information (Title,
#'     Description, *ToDo: add fields*).
#' @param class Assay class key string.
#'     Possible values are 'EXP' and 'MODEL'.
#' @param file File name with path, relative to layer.
#' @return FAIRDOMhub created component.
#' @note Upon success (status code 200) details
#' of newly created component can be used. Check status code.
#' @export
#' @keywords pisa
#' @seealso \code{\link{skGet}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' if(FALSE)
#' {
#' skIni(prid = 26, test=TRUE)
#' options("seekr")
#'  sp <- skCreate( type = "projects"
#'   , meta= list(
#'       Title=paste("Test project", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  str(sp)
#'  str(skData(sp))
#' # Add member manually
#
#'  skIni(prid = 26, pid=104, test=TRUE)
#'  options("seekr")
#'  si <- skCreate( type = "investigations"
#'   , meta= list(
#'       Title=paste("Test investigation", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  si
#'  skData(si)$id
#'
#'  iid=skData(si)$id
#'  iid <- 115
#'  skIni(prid = 26, pid=104, iid=iid, test=TRUE)
#'  options("seekr")
#'  ss <- skCreate( type = "studies"
#'   , meta= list(
#'       Title=paste("Test study", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  ss
#'  skData(ss)$id
#'
#'  skIni(prid = 26
#'        , pid=104
#'        , iid=skData(si)$id
#'        , sid=skData(ss)$id
#'        , test=TRUE)
#'  options("seekr")
#'  sa <- skCreate( type = "assays"
#'   , meta= list(
#'       Title=paste("Test assay", Sys.time())
#'     , Description="Testing of upload")
#'     , class="EXP"
#'     )
#'  #str(sa)
#'  sa
#' }
#'
#' # Type: data_file
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/"
#' oldwd <- setwd(system.file("extdata",astring,package="seekr"))
#' oldwd
#' .aname <- getLayer("A")
#' .aroot <- getRoot("A")
#' .ameta  <- readMeta()
#'  file <- "input/README.MD"
#'  skIni(prid = 26, pid=104, iid=115 , sid=117 , aid=401, test=TRUE)
#'  type <- "data_files"
#'  type <- "documents"
#'  sdat <- skCreate( type = type
#'   , meta= list(
#'       Title=paste("Test assay", Sys.time())
#'     , Description="Testing of upload")
#'   , file=file
#'     )
#'  #str(sdat)
#'  sdat
#'  skData(sdat)$id
#' if(interactive()) setwd(oldwd)
#' getwd()
#' res <- sdat$content
#' item_link <- file.path(res$meta$base_url,res$links$self)
#' if(interactive()) shell.exec(item_link)
#' }
skCreate <- function (type = "assays", meta=list(), class="EXP", file="NA.TXT"){
     myid <- options()$seekr$myid
     prid <- options()$seekr$prid
     pid <-  options()$seekr$pid
     iid <-  options()$seekr$iid
     sid <-  options()$seekr$sid
     aid <-  options()$seekr$aid
     s <- skSkeleton(type, meta)
# Common fileds
     s$data$attributes$title  <-  getMeta(meta,"Title")
     s$data$attributes$description  <-  getMeta(meta, "Description")
# Type specific fields
     switch(s$data$type,
     projects = {
#       s$data$attributes$members$person_id <- myid
#       s$data$attributes$members$institution_id <- 69
       s$data$attributes$default_policy$permissions$resource$id <- myid
       s$data$relationships$programmes$data$id <- prid
       s$data$relationships$creators$data$id <- prid

       s$data$relationships$project_administrators$data$id <- myid
       s$data$relationships$asset_housekeeper$data$id <- myid
            }
     , investigations = {
        s$data$relationships$projects$data$id <- pid
        s$data$attributes$policy$permissions$resource$id <- pid
        s$data$relationships$creators$data$id <- myid
        s$data$relationships$creators$data$type <- "people"
            }
     , studies = {
        s$data$relationships$investigation$data$id <-iid
        s$data$attributes$person_responsible_id <- myid
        s$data$attributes$policy$permissions$resource$id <- pid
        s$data$relationships$creators$data$id <- myid
        s$data$relationships$creators$data$type <- "people"
            }
     , assays = {
        s$data$relationships$study$data$id <-sid
        s$data$attributes$person_responsible_id <- myid
        s$data$attributes$policy$permissions$resource$id <- pid
        s$data$relationships$creators$data$id <- myid
        s$data$relationships$creators$data$type <- "people"
        s$data$attributes$assay_class$title <- ""
        s$data$attributes$assay_class$key <- toupper(class)
#        s$data$attributes$assay_class$key <- "MODEL"
        s$data$attributes$assay_class$description <- ""
        s$data$attributes$assay_type$label <- ""
        s$data$attributes$assay_type$key <- ""
            }
      , data_files = {
        s$data$attributes$title  <-  paste0("/",file)
        s$data$attributes$description  <-  ""
        s$data$attributes$tags  <- c("data","demo")
        s$data$attributes$content_blobs$original_filename <- paste0("/",file)
        s$data$attributes$content_blobs$content_type <- contentType(file)
        s$data$attributes$policy$permissions$resource$id <- pid
        s$data$relationships$creators$data$id <- myid
        s$data$relationships$projects$data$id <- pid
        s$data$relationships$assays$data$id <- aid
      }
      , documents = {
        document_title <- paste0("/",file)
        document_description <- ""
        content_type <- contentType(file)
        tags <- content_type
        #
        fpath <- file.path(".", file)
        #
        s$data$attributes$title <- document_title
        s$data$attributes$description <- document_description
        s$data$relationships$creators$data$id <- myid
        s$data$relationships$creators$data
        s$data$relationships$projects$data$id <- pid
        s$data$relationships$projects$data
        s$data$relationships$assays$data$id <- aid
        s$data$relationships$assays$data
        s$data$attributes$policy$permissions$resource$id <- pid
        s$data$attributes$policy$permissions
        s$data$attributes$content_blobs$content_type <- content_type
        s$data$attributes$content_blobs$original_filename <- document_title
        s$data$attributes$content_blobs
        s$data$attributes$other_creators <- ""
        s$data$attributes$tags <- c("pISA", tags)
      }
     )
     baseurl <- options()$seekr$baseurl
     uri <- httr::modify_url(baseurl,path=s$data$type)
     I <- jsonlite::toJSON(s, auto_unbox=TRUE, pretty=TRUE)
     #
     # For testing: list JSON object
     #     cat(I, "\n")
     #
     ua <- httr::user_agent("https://github.com/nib-si/seekr")
     skLog("skCreate", uri)
     fht <- system.time(resp <- httr::POST(uri
         , authenticate(
             options()$seekr$usr
           , options()$seekr$pwd
           , type = "basic")
         , body = I
         , encode="json"
         , accept("application/json")
         , content_type_json()
         , ua
         )
         )
     parsed <- skParse(resp)
     skLog( resp$headers$status, round(fht["elapsed"],2), parsed$url)
# Upload file blob
     if(FALSE&&s$data$type %in% c("data_files", "documents")){
     skLog("Upload file:", file)
     res <- parsed$content
     original_filename <- res$attributes$content_blobs[[1]]$original_filename
     blob_link <- res$attributes$content_blobs[[1]]$link
     # Fill the content blob with data
     uri <- blob_link
     fpath <- file.path(".",.aroot, file)
     if(file.exists(fpath)) {
     skLog("skUpload",uri)
     fht <- system.time(
     resp_blob <- httr::PUT(uri
         , authenticate(
             options()$seekr$usr
           , options()$seekr$pwd
           , type = "basic"
           )
    , body = upload_file(fpath)
    , encode="json"
    , accept("*.*")
    ))
    resp_blob <- jsonlite::fromJSON(content(resp_blob, as = "text"))
    resp_blob <- paste(fpath, " | File size:", resp_blob)
    } else {
    resp_blob <- paste("Error: File not found:" ,fpath)
    skLog( resp_blob$headers$status
         , round(fht["elapsed"],2)
         , parsed$url)
     }
     }
     parsed
}
#skCreate("documents",meta,file=file)



## ----skUpload-------------------------------------------------------
#' Upload file.
#'
#' Upload file to a crerated object of type 'documents' or 'data_files'.
#'
#' @param object A previously created SEEK component.
#'     Must be one of 'documents' or 'data_files'.
#' @param file File name with path, relative to layer.
#' @return FAIRDOMhub created component.
#' @note Upon success (status code 200) details
#' of newly created component can be used. Check status code.
#' @export
#' @keywords pisa
#' @seealso \code{\link{skCreate}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' file <- dir()[1]
#' file
#' fhd <- skCreate("documents",meta,file=file)
#' fhd
#' sd <- skUpload(fhd, file)
#'
skUpload <- function( object, file){
     if(object$content$type %in% c("data_files", "documents")){
     skLog("Upload file:", file)
     res <- object$content
     original_filename <- res$attributes$content_blobs[[1]]$original_filename
     blob_link <- res$attributes$content_blobs[[1]]$link
     # Fill the content blob with data
     uri <- blob_link
     fpath <- file.path(".",file)
     if(file.exists(fpath)) {
     skLog("skUpload",uri)
     fht=-1
     fht <- system.time(
     resp <- httr::PUT(uri
        , authenticate(
             options()$seekr$usr
           , options()$seekr$pwd
           , type = "basic"
        )
    , body = upload_file(fpath)
    , encode="json"
    , accept("*.*")
    ))
    print(resp)
    fs <- round(as.numeric(rawToChar(resp$content))/1024^2,3)
    file_size <- paste0(fpath, " | ", fs,"MB")
    } else {
    resp_blob <- paste("Error: File not found:" ,fpath)
    }
    skLog( resp$headers$status
         , round(fht["elapsed"],2)
         , file_size
         , object$url)
}
  resp
}

