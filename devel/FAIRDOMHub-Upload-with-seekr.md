---
title: "Upload to FAIRDOMHub with seekr"
author: "A. Blejec"
date: "13. 05. 2021"
output: html_document
---



## Introduction

I will organize a batch upload to FAIRDOMHub. For the operation I have prepared the R package `seekr`, available at [GitHub](https://github.com/NIB-SI/seekr).


```r
library(pisar)
library(rio)
library(jsonlite)
```

```
## Warning: package 'jsonlite' was built under R version 4.0.3
```

```r
library(httr)
library(seekr)
```


## Prepare pISA environment

Package `seekr` contains demo pISA-tree project. For this demonstration we can use the demo project. It is assumed, that our working directory is a layer of the pISA project, in our case we will start in an assay.


```r
astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/"
oldwd <- setwd(system.file("extdata", astring, package="seekr"))
oldwd
```

```
## [1] "C:/__D/OMIKE/seekr/devel"
```

```r
dir()
```

```
##  [1] "_Assay_METADATA.TXT" "_outputFile.R"       "FAIRDOM.log"        
##  [4] "input"               "other"               "other.txt"          
##  [7] "output"              "README.MD"           "reports"            
## [10] "scripts"             "showMetadata.bat"    "showTree.bat"       
## [13] "xcheckMetadata.bat"
```


Initialize the pISA-tree. Function `pisa` will complain, if the working directory is not a pISA layer.


```r
pini <- pisa()
```

```
## Error in getRoot("p", path): Path is not within a pISA-tree:
##  C:/__D/OMIKE/seekr/devel
```

Let's check some of the infos gathered:

Assay name:

```r
.aname
```

```
## Error in eval(expr, envir, enclos): object '.aname' not found
```

```r
# or
pini$A$name
```

```
## Error in eval(expr, envir, enclos): object 'pini' not found
```

Study information


```r
pini$S
```

```
## Error in eval(expr, envir, enclos): object 'pini' not found
```

Study and project descriptions

```r
getMeta(.smeta,"Description")
```

```
## Error in is.data.frame(x): object '.smeta' not found
```

```r
getMeta(.pmeta,"Description")
```

```
## Error in is.data.frame(x): object '.pmeta' not found
```


## Prepare study files

SEEK supports only files in assays. Study files need to be copied to an assay.



```r
snames <- dir(.iroot, pattern="^_S_"
              , full.names = FALSE
              , recursive=FALSE
              , include.dirs=TRUE)
```

```
## Error in dir(.iroot, pattern = "^_S_", full.names = FALSE, recursive = FALSE, : object '.iroot' not found
```

```r
snames
```

```
## Error in eval(expr, envir, enclos): object 'snames' not found
```

```r
#snames <- snames[grepl("^_S_",snames)]
for(sname in snames) {
  asname <- paste0("_A_",sname,"-files")
  aspath <- file.path(.iroot,sname,asname)
  if(!dir.exists(aspath)) dir.create(aspath)
  #file.copy(file.path(.iroot,sname,"*.*"), aspath)
}
```

```
## Error in eval(expr, envir, enclos): object 'snames' not found
```

Get the directory listing of all studies in the parent investigation
and exclude files listed in file `seekignore.txt`. In our case, seekignore file is in the investigation layer.


```r
# Get SEEK ignore file
ignore_file <- file.path(.iroot,"seekignore.txt")
```

```
## Error in file.path(.iroot, "seekignore.txt"): object '.iroot' not found
```

```r
if(file.exists(ignore_file))
seekignore <- readLines(ignore_file) else seekignore <- ""
```

```
## Error in file.exists(ignore_file): object 'ignore_file' not found
```

```r
seekignore
```

```
## Error in eval(expr, envir, enclos): object 'seekignore' not found
```

```r
# Create directory listing

dirs <-  dir(.iroot
             , full.names = FALSE
             , recursive=TRUE
             , include.dirs=FALSE)
```

```
## Error in dir(.iroot, full.names = FALSE, recursive = TRUE, include.dirs = FALSE): object '.iroot' not found
```

```r
head(dirs,10)
```

```
## Error in head(dirs, 10): object 'dirs' not found
```

```r
length(dirs)
```

```
## Error in eval(expr, envir, enclos): object 'dirs' not found
```

```r
# Ignore files with patterns in seekignore file
dirlength <- length(dirs)
```

```
## Error in eval(expr, envir, enclos): object 'dirs' not found
```

```r
for(pat in toupper(seekignore)){
  ldir <- length(dirs)
  at <- gsub("\\.","\\\\.",pat)
  dirs <- dirs[!grepl(pat,toupper(dirs))]
  cat("Ignoring" , ldir-length(dirs), "files [",pat," ]\n")
}
```

```
## Error in toupper(seekignore): object 'seekignore' not found
```

```r
cat("Files to upload:", length(dirs), "/",dirlength)
```

```
## Error in cat("Files to upload:", length(dirs), "/", dirlength): object 'dirs' not found
```

```r
# Keep only files in Assays

head(dirs)
```

```
## Error in head(dirs): object 'dirs' not found
```

```r
dirs <- dirs[grepl("/_A_", dirs)]
```

```
## Error in eval(expr, envir, enclos): object 'dirs' not found
```

```r
head(dirs)
```

```
## Error in head(dirs): object 'dirs' not found
```

```r
tail(dirs)
```

```
## Error in tail(dirs): object 'dirs' not found
```

```r
cat("Files to upload:", length(dirs), "/",dirlength)
```

```
## Error in cat("Files to upload:", length(dirs), "/", dirlength): object 'dirs' not found
```

## Prepare connection with FAIRDOMHub

Restart the log file.


```r
skLog("Starting upload for demo project", append=FALSE)
```

Initialize connection with SEEK cloud service. Cloud service url and project creator name and SEEK service id are expected in the project metadata file. They should be stored in options().


```r
options(.sk$test)
# options(.sk$main)
skOptions()
```

```
## Hidden: sk.pwd
```

```
## $sk.myid
## [1] 368
## 
## $sk.url
## [1] "https://testing.sysmo-db.org"
## 
## $sk.usr
## [1] "ablejec"
```


Here I am showing a part of the settings (to not reveal my password).

Find creator's FAIRDOMHub id. Project creator name should be spelled exactly as in the FAIRDOMHub account.


```r
meta <- pini$p$meta
```

```
## Error in eval(expr, envir, enclos): object 'pini' not found
```

```r
crid <- skFindId("people",meta[grepl("creator",meta$Key),]$Value)["id"]
```

```
## Status code: 200
```

```
## Error in pmatch(title, titles[, "title"]): object 'meta' not found
```

```r
crid
```

```
## Error in eval(expr, envir, enclos): object 'crid' not found
```

```r
crid <- skFindId("people","Andrej Blejec")["id"]
```

```
## Status code: 200
```

```r
crid
```

```
##    id 
## "368"
```

```r
if(crid == 0 ) crid <- 368
myid <- crid
myid
```

```
##    id 
## "368"
```



```r
options(sk.myid = crid)
```

I can also get the institution. SEEK API returns a JSON structure, which is converted into an R list. One can traverse the list to get to the relevant data.


```r
resp <- skContent( skGet("people",  getOption( "sk.myid" ) ) )$relationships$institutions$data[[1]]
```

```
## Status code: 200
```

```r
resp
```

```
## $id
## [1] "69"
## 
## $type
## [1] "institutions"
```

```r
str(resp)
```

```
## List of 2
##  $ id  : chr "69"
##  $ type: chr "institutions"
```

```r
skGet(resp$type,resp$id)
```

```
## Status code: 200
```

```
## Status: 200 
## Object: https://testing.sysmo-db.org/institutions/69 
## Path  : /institutions/69 
## Title : National Institute of Biology
```















