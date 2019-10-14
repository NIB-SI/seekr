# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

## ----fileType------------------------------------------------------------
#' Extract file type
#'
#' Extract file type from a file path.
#'
#' @param x file path and file name
#' @param ... any other arguments (not treated at the moment
#' @return File type (string)
#' @export
#' @note
#' @references
#' @keywords package
#' @title
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileType(".\\validation.Rnw")
fileType <- function(x,...){
  type <- gsub("(.*)\\.(.*)","\\2",basename(x))
  if(type==x)  type  <- ""
  return(type)
}
fileType("bla")
fileType("./bla.Rnw")
