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
#' @notes The returned list is added to the 
#'      \code{options()} list under name 'fhub'.
#' @export
#' @keywords file
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' fhIni()
#' options("fhub")
#' #
#' fhIni(test=FALSE)
#' options("fhub")
#' }
fhIni <- function(
      prid = NULL
    , pid = NULL
    , iid = NULL
    , sid = NULL
    , aid = NULL
    , test=TRUE){
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
tmp <- list(baseurl = baseurl
   , usr = usr
   , pwd = pwd
   , myid = as.character(myid)
   , prid = as.character(prid)
   , pid = as.character(pid)
   , iid = as.character(iid)
   , sid = as.character(sid)
   , aid = as.character(aid)
   )
   options(fhub = tmp)
invisible(tmp)
}


## ----fhParse-------------------------------------------------------------
#' Parse the response from SEEK API
#'
#' @param resp Response from SEEK API.
#' @return An object (list) of class \code{seek_api}.
#' @export
#' @seealso \code{\link{fhGget ...}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' options()$fhub$myid
#' r <- fhGet("people",options()$fhub$myid)
#' names(r)
#' r$response$status_code
#' status_code(r$response)
#' names(r$content)
#' r
#' r <- fhGet("people")
#' length(r$content)
#' names(r$content)
#' r$content[[1]]
#' r
#' }
fhParse <- function(resp, ...){
    jsn <- "application/json"
    parsed <- invisible(content(resp,"parsed",type=jsn)$data)
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
  url <- modify_url(parsed$meta$base_url,path=parsed$links)
  ret <-    structure(
       list(
         path = parsed$links,
         url = url,
         content = parsed,
         response = response
       ),
       class = "seek_api"
       )
  return(ret)
}


## ----print.seek_api------------------------------------------------------
#' Print method for seek_api object
#'
#' @param x Object of class \code{seek_api}.
#' @param content If FALSE (default), content is no printed.
#' @return An object (list) of class \code{seek_api}.
#' @export
#' @seealso \code{\link{fhParse ...}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' options()$fhub$myid
#' r <- fhGet("people",options()$fhub$myid)
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
cat("Path  :", x$path$self,"\n")
cat("Title :", x$content$attributes$title, "\n")
if(content) {
cat(rep("-",20),"\n\n")
 str(x$content)
 }
invisible(x)
}
}


## ----fhGet---------------------------------------------------------------
#' Get inormation from repository.
#'
#' @param type Type of information (e.g. "person").
#' @param id Repository id of an item.
#' @param uri Repository base address (URI)..
#' @return An object (list) of class \code{seek_api}.
#' @export 
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{get ...}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' fhIni()
#' options()$fhub$myid
#' r <- fhGet("people",options()$fhub$myid)
#' names(r)
#' r$response$status_code
#' status_code(r$response)
#' r
#' }
fhGet <- function(type, id,
                   uri=options()$fhub$baseurl, ... ){
#                  uri="https://www.fairdomhub.org", ... ){
  if(!missing(type)) uri <- paste0(uri,"/",type)
  if(!missing(id)) uri <- paste0(uri,"/",id)
  resp <- GET(uri,
         add_headers(Accept="application/json"))
  # cat("Status code:",r$status_code,"\n")
  parsed <- fhParse(resp)
  return(parsed)
}
#



## ----fhData--------------------------------------------------------------
#' Get content from an *fh* object.
#'
#' @param r Object retrieved by fhGet.
#' @param type Name of the required element. If missing, a list with
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
#' r <- fhGet("people",options()$fhub$myid)
#' d <- fhData(r,"attributes")
#' names(d)
#' d$last_name
#' fhData(r)$tools
#' # Get list of people
#' r <- fhGet("people")
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
fhData <- function(r, node, ...){
  if(class(r)=="seek_api") r <- r$response
  jsn <- "application/json"
  if(missing(node)) invisible(content(r,"parsed",type=jsn)$data) else
  invisible(content(r,"parsed",type=jsn)$data[[node]])
}
fhData <- function(r, node, ...){
  if(class(r)=="seek_api") r <- r$content
  jsn <- "application/json"
  if(missing(node)) invisible(r) else
  invisible(r[[node]])
}
fhDatas <- fhData


## ----fhFindId------------------------------------------------------------
#' Get details of component with id from an *fh* object.
#'
#' @param type Components name (e.g. 'people', 'projets', ...).
#' @param title Character string with the identifier
#'     of the component (title part).
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
fhFindId <- function(type, title){
     r <- fhGet(type)
     d <- fhData(r)
     titles <- t(sapply(d,function(x) c(id=x$id, type=x$type, title=x$attributes$title)))
     # Get FAIRDOMhub user id
     if(!missing(title)){
       id <- d[[pmatch(title,titles[,"title"])]]$id
       return(c(id=id,type=type, title=title))
     } else {
     return(titles)
     }
}


## ----fhFindTitle---------------------------------------------------------
#' Get details of component with id from an *fh* object.
#'
#' @param type Components name (e.g. 'people', 'projets', ...).
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
fhFindTitle <- function(type, id){
     id <- as.vector(id[1])
     r <- fhGet(type, id )
     d <- fhData(r, "attributes")
     # Set FAIRDOMhub user title
     return(c(id=id, type=type, title=d$title))
     }


## ----fhSkeleton----------------------------------------------------------
#' Create *fh* skeleton.
#'
#' Creates *fh* object with required structure.
#'
#' @param type Component name (e.g. 'people', 'projets', ...).
#' @param meta Data frame with pISA metadata or
#' a list with minimal information (Title, Description, *ToDo: add fields*).
#' @return A list with the minimal information structure.
#' @export
#' @keywords pisa
#' @seealso \code{\link{fhCreate}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' require(jsonlite)
#' meta= list(Title = "Test layer", Description = "Some description")
#' type = "projects"
#' sp <- fhSkeleton( type = type
#'   , meta= meta
#'   )
#' str(sp)
#' type = "investigations"
#' si <- fhSkeleton( type = type
#'   , meta= meta 
#'   )
#' str(si)
#' type = "studies"
#' ss <- fhSkeleton( type = type
#'   , meta= meta
#'   )
#' str(ss)
#' type = "assays"
#' sa <- fhSkeleton( type = type
#'   , meta= meta
#'   )
#' str(sa)
#' }
#' }
fhSkeleton <- function (type = "assay", meta){
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
, '{"Error":"No such type"}'
)
# keep uncommented lines
sx <- unlist(strsplit(sj,"\n"))
sx <- sx[!grepl("^#",sx)]
sj <- paste(sx,collapse="\n")
sr <- fromJSON(sj, simplifyVector = TRUE)
return(sr)
}



## ----fhCreate------------------------------------------------------------
#' Create pISA layer or *fh* component.
#'
#' @param type Component name (e.g. 'people', 'projets', ...).
#' @param meta Data frame with pISA metadata or
#' a list with minimal information (Title, Description, *ToDo: add fields*).
#' @return FAIRDOMhub created component.
#' @note Upon success (status code 200) details
#' of newly created component can be used. Check status code.
#' @export
#' @keywords pisa
#' @seealso \code{\link{fhGet}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' if(FALSE)
#' {
#' fhIni(prid = 26, test=TRUE)
#' options("fhub")
#'  sp <- fhCreate( type = "projects"
#'   , meta= list(
#'       Title=paste("Test project", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  str(sp)
#'  str(fhData(sp))
#' # Add member manually
#
#'  fhIni(prid = 26, pid=104, test=TRUE)
#'  options("fhub")
#'  si <- fhCreate( type = "investigations"
#'   , meta= list(
#'       Title=paste("Test investigation", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  si
#'  fhData(si)$id
#' }
#'  iid=fhData(si)$id
#'  iid <- 115
#'  fhIni(prid = 26, pid=104, iid=iid, test=TRUE)
#'  options("fhub")
#'  ss <- fhCreate( type = "studies"
#'   , meta= list(
#'       Title=paste("Test study", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  ss
#'  fhData(ss)$id
#'
#'  fhIni(prid = 26
#'        , pid=104
#'        , iid=fhData(si)$id
#'        , sid=fhData(ss)$id
#'        , test=TRUE)
#'  options("fhub")
#'  sa <- fhCreate( type = "assays"
#'   , meta= list(
#'       Title=paste("Test assay", Sys.time())
#'     , Description="Testing of upload")
#'     )
#'  #str(sa)
#'  sa
#'  fhData(sa)$id
#' }
fhCreate <- function (type = "assays", meta){
     myid <- options()$fhub$myid
     prid <- options()$fhub$prid
     pid <-  options()$fhub$pid
     iid <-  options()$fhub$iid
     sid <-  options()$fhub$sid
     s <- fhSkeleton( type, meta)
# Common fileds
     s$data$attributes$title  <-  getMeta(meta,"Title")
     s$data$attributes$description  <-  getMeta(meta, "Description")
     #
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
        s$data$attributes$assay_class$key <- "EXP"
#        s$data$attributes$assay_class$key <- "MODEL"        
        s$data$attributes$assay_class$description <- ""
        s$data$attributes$assay_type$label <- ""
        s$data$attributes$assay_type$key <- ""
            }
     )
     baseurl <- options()$fhub$baseurl
     uri <- modify_url(baseurl,path=s$data$type)
     I <- toJSON(s, auto_unbox=TRUE, pretty=TRUE)
     cat(I, "\n")
     resp <- POST(uri
         , authenticate(
             options()$fhub$usr
           , options()$fhub$pwd,type = "basic")
         , body = I
         , encode="json"
         , accept("application/json")
         , content_type_json()
         )
     fhParse(resp)
     }

