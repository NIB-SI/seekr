## ----include=FALSE-------------------------------------------------------
#outputFile <- "report-copy.pdf"
library(knitr)
render_sweave()
opts_chunk$set(fig.path='./figs/',tidy=TRUE)
opts_knit$set(concordance=TRUE)

## ----Author:,echo=FALSE--------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################

## ----initialize,echo=FALSE,results='hide',message=FALSE-------------
options(width=70)
#library(Hmisc)
#library(xlsReadWrite)

## -------------------------------------------------------------------
library(pisar)

## -------------------------------------------------------------------
astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"

## -------------------------------------------------------------------
.pisaPath <- system.file("extdata",astring,package="pisar")
oldwd <- getwd()
if(interactive()) {
    oldwd <- setwd(.pisaPath)
    strsplit(getwd(),"/")
    }

## -------------------------------------------------------------------
opts_knit$set(root.dir = .pisaPath)

## -------------------------------------------------------------------
dir()
readLines("README.MD")

## -------------------------------------------------------------------
.proot <- getRoot("p")
.proot
dir(.proot,pattern=glob2rx("*.TXT"))

## -------------------------------------------------------------------
.iroot <- getRoot("I")
.iroot
dir(.iroot,pattern=glob2rx("*.TXT"))

## -------------------------------------------------------------------
.sroot <- getRoot("S")
.sroot
dir(.sroot,pattern=glob2rx("*.TXT"))

## -------------------------------------------------------------------
.aroot <- getRoot("A")
.aroot
dir(.aroot,pattern=glob2rx("*.TXT"))

## ----readMeta1------------------------------------------------------
.proot
.pmeta <- readMeta(.proot)

## ----structurep-----------------------------------------------------
str(.pmeta)

## ----projectmeta----------------------------------------------------
.pmeta


## ----othermeta------------------------------------------------------
(.imeta <- readMeta(.iroot))
(.smeta <- readMeta(.sroot))
(.ameta <- readMeta(.aroot))


## -------------------------------------------------------------------
getMeta(.pmeta,"Title")
getMeta(.pmeta,"Title:")

## -------------------------------------------------------------------
getMeta(.ameta,"Description")

## -------------------------------------------------------------------
p <- pisa()

## -------------------------------------------------------------------
p$A

## -------------------------------------------------------------------
p$S$root

## -------------------------------------------------------------------
ls(pattern="^\\.",all.names=TRUE)

## -------------------------------------------------------------------
opts_knit$set(root.dir = oldwd)
if(interactive()) setwd(oldwd)

## ----sessionInfo,results='asis',echo=FALSE--------------------------
cat(win.version(),"\n")
toLatex(sessionInfo())

