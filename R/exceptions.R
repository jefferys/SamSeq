# Exceptions.R - Exceptions and associated routines
#
# Probably should be split out as a separate package at some point, but
# needs some work involving non-stantdard evaluations a-la assertthat
# https://github.com/hadley/assertthat and some work on easier creation of
# exception class hierarchies. non-standard evaluation can help both with
# bundling tests and the exception they throw and in handling exception
# parameters.

#' Create exception condition objects.
#'
#' Creates exception condition object of various classes. Exceptions come in
#' hierarchies to allow handling any exception in a group. All exceptions
#' inherit from condition, and hence have a message (accessible with
#' \code{conditionMessage}) and a call,() usually NULL, accessible with
#' \code{conditionCall}). However the base class should be considered to be
#' "Exception". Some exceptions have additional required parameters, such as
#' FileException, which requires a \code{path} parameter. These parameters are
#' required in all child exceptions too.
#'
#' You can call stop, warning, and message on any exception object, and catch
#' the exception with a try/catch condition handler.
#'
#' If exceptions are visible to users (because they were signaled by an
#' exception passed to stop, warning, message, etc) these are part of a packages
#' public API just like function calls. They should be clearly documented and
#' generally can be changed only in a backward-compatible manner. User code that
#' catches exceptions depend on them being thrown with the same class name and
#' having both the same parent classes and the same extra parameters.
#'
#' @param message The message associated with the exception. Generally should be
#'   more informative than just the exception class name, which is used by
#'   default in base exception classes.
#' @param call The call stack at the point where this exception was constructed.
#'   Generally this is not provided (it is \code{NULL}) as the message should be
#'   informative, not just a call stack trace.
#' @param path The path associated with a FileException.
#'
#' @return An exception object in the class hierarchy:
#' \itemize{
#' \item{\code{                                                 condition}}
#' \item{\code{                                      Exception, condition}}
#' \item{\code{                       FileException, Exception, condition}}
#' \item{\code{FileNotFoundException, FileException, Exception, condition}}
#' }
#'
#' @name Exception
#' @export
condition <- function( message= "condition", call= NULL ) {
   structure( class= "condition",
              list( message= message, call= call ))
}

#' @rdname Exception
#' @export
Exception <- function ( message='Exception', call= NULL ) {
   structure( class= c("Exception", "condition"),
              list( message= message, call= call ))
}

#' @rdname Exception
#' @export
FileException <- function ( path, message='FileException', call= NULL ) {
   structure( class= c( "FileException", "Exception", "condition" ),
              list( path= path, message= message, call= call ))
}

#' @rdname Exception
#' @export
FileNotFoundException <- function ( path, message= NULL, call= NULL ) {
   if ( is.null( message )) {
      message <- paste0( "File not found: \"", path, "\" (looking in: \"", getwd(), "\")." )
   }
   structure( class= c("FileNotFoundException", "FileException", "Exception", "condition" ),
              list( path=path, message= message, call= call ))
}

#' Get the path associated with a file system exception.
#'
#' @param e The file system exception to query
#'
#' @return The path of the thing a file system exception is referring to as a 1
#'   element character vector.
#'
#' @seealso FileSystemException, conditionMessage, conditionCall
#'
#' @export
path <- function (...) {
   UseMethod( "path" )
}

#' @rdname path
#' @export
path.FileException <- function (e, ...) { return (e$path) }
