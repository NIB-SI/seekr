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
#' @seealso \code{\link{fhGET}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \donotrun{
#' si <- fhCreate( type = "investigations"
#'   , meta= list(
#'       Title="Test Investigation"
#'     , Description="Some description")
#'     )
#'  str(si)
#'  sp <- fhCreate( type = "projects"
#'   , meta= list(
#'       Title="Test Project"
#'     , Description="Some description")
#'     )
#'  str(sp)
#' }
fhCreate <- function (type = "assays", meta){
     s <- fhSkeleton( type, meta)
     s$data$attributes$title  <-  getMeta(meta,"Title")
     s$data$attributes$description  <-  getMeta(meta, "Description")
     s$data$relationships$creators$data$id <- myid
     #
     switch(s$data$type,
     investigations = {
             s$data$relationships$projects$data$id <- pid
             s$data$attributes$policy$permissions$resource$id <- pid
             }
     , projects = {
             s$data$relationships$programmes$data$id <- 26
             s$data$attributes$policy$permissions$resource$id <- pid
             }
     )
     uri <- modify_url(baseurl,path=s$data$type)
     I <- toJSON(s, auto_unbox=TRUE)
     print(str(s))
     print(I)
     response <- POST(uri
         , authenticate("ablejec","testni.1234",type = "basic")
         , body = I
         , encode="json"
         , accept("application/json")
         , content_type_json()
         )
     print(response$status)
     #res <- fromJSON(content(response, as = "text"))
     return(response)
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
#' type = "investigations"
#' meta= list(Title = "TestAssay", Description = "Some description")
#' sr <- fhSkeleton( type = type
#'   , meta= meta 
#'   )
#' str(sr)
#' }
fhSkeleton <- function (type = "assay", meta){
sj <- switch( type
, projects =
'{
  "data": {
    "type": "projects",
    "attributes": {
      "avatar": null,
      "title": "*",
      "description": "*",
      "web_page": "",
      "wiki_page": "",
      "default_license": "GPLv3",
      "default_policy": {
        "access": "view",
        "permissions": [
          {
            "resource": {
              "id": "*",
              "type": "people"
            },
            "access": "manage"
          },
          {
            "resource": {
              "id": "*",
              "type": "projects"
            },
            "access": "edit"
          },
          {
            "resource": {
              "id": "",
              "type": "institutions"
            },
            "access": "view"
          }
        ]
      }
    },
    "relationships": {
      "programmes": {
        "data": [
          {
            "id": "*",
            "type": "programmes"
          }
        ]
      },
      "organisms": {
        "data": [
          {
            "id": "*",
            "type": "organisms"
          }
        ]
      }
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
            "access": "view"
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
      },
      "publications": {
        "data": [
          {
            "id": "",
            "type": "publications"
          }
        ]
      },
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
, assays =
'{
  "data": {
    "type": "assays",
    "attributes": {
      "title": "A Maximal experimental Assay",
      "assay_class": {
        "key": "EXP"
      },
      "assay_type": {
        "uri": "http://jermontology.org/ontology/JERMOntology#Transcriptomics"
      },
      "technology_type": {
        "uri": "http://jermontology.org/ontology/JERMOntology#RNA-Seq"
      },
      "other_creators": "Anonymous creator",
      "description": "A Western Blot Assay",
      "policy": {
        "access": "download",
        "permissions": [
          {
            "resource": {
              "id": "442",
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
          "id": "66",
          "type": "studies"
        }
      },
      "publications": {
        "data": [
          {
            "id": "16",
            "type": "publications"
          }
        ]
      },
      "organisms": {
        "data": [
          {
            "id": "3",
            "type": "organisms"
          }
        ]
      },
      "sops": {
        "data": [
          {
            "id": "4",
            "type": "sops"
          }
        ]
      },
      "models": {
        "data": [
          {
            "id": "5",
            "type": "models"
          }
        ]
      },
      "data_files": {
        "data": [
          {
            "id": "16",
            "type": "data_files"
          }
        ]
      },
      "documents": {
        "data": [
          {
            "id": "32",
            "type": "documents"
          }
        ]
      },
      "creators": {
        "data": [
          {
            "id": "287",
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
, '{"Error":"No such component"}'
)
# keep uncommented lines
sx <- unlist(strsplit(sj,"\n"))
sx <- sx[!grepl("^#",sx)]
sj <- paste(sx,collapse="\n")
sr <- fromJSON(sj, simplifyVector = TRUE)
return(sr)
}




