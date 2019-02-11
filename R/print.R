#' Preprocessor printing
#' 
#' This simply prints out a length 1 character vector specifying the type of
#' preprocessor.
#' 
#' @param x A preprocessor.
#' 
#' @export
preprocessor_type <- function(x) UseMethod("preprocessor_type")

#' @export
preprocessor_type.default <- function(x) "None"

#' @export
preprocessor_type.default_preprocessor <- function(x) "Default"

#' @export
preprocessor_type.recipe <- function(x) "Recipe"

#' @export
preprocessor_type.terms <- function(x) "Formula"