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
Exception <- function ( message='Exception', call= NULL, ... ) {
   extendException( "Exception", base= condition(), message=message, call=call, ... )
}

#' @rdname Exception
#' @export
FileException <- function ( path, message=NULL, call= NULL, ... ) {
   if (is.null(path)) {path <- "<path not specified>"}
   if (is.null(message)) {
      message=paste0( "File Exception: \"", path, "\" (looking in: \"", getwd(), "\").")
   }
   extendException( "FileException", base= Exception(),
                    message=message, call=call, path= path, ... )
}

#' @rdname Exception
#' @export
FileNotFoundException <- function ( path,
   message= paste0( "File not found: \"", path, "\" (looking in: \"", getwd(), "\")." ),
   call= NULL, ...
) {
   extendException( "FileNotFoundException", base= FileException(path),
                    message= message, call= call, ... )
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

#' Create a new exception that inherits from an existing one.
#'
#' This is used to create exception constructors. Exceptions are based off of an
#' existing exception, usually constructed inline. This base exception is then
#' used to create a new exception. The message, call, and and data properties
#' are copied into the new exception, but can be over-ridden by any new message,
#' call or data properties specified. Essentially any \code{name= value}
#' properties can be included by specifying them as arguments, except for the
#' already-used argument names \code{exception}, \code{base}, \code{message},
#' and \code{call}
#'
#' @section Efficiency considerations: Building child class instances using
#'   parent class instances is not normally done in \R due to its copy-by-value
#'   semantics. However questions involving the efficiency of even a
#'   relatively long chain of exceptions built using nested calls to
#'   \code{extendException} are irrelevant because exceptions should be
#'   generated rarely and not used for routine flow control.
#'
#' @param exception A one-element character vector giving the new exception's
#'   class name.
#' @param base An existing exception to base this exception off of. Often
#'   constructed in-line to provide needed parameters. By default this is the
#'   exception generated by a no-parameter call to "Exception()."
#' @param message The message for this exception, by default the message from
#'   the base exception which is used when this is \code{NULL}.
#' @param call The call parameter for this exception, by default the call from
#'   the base exception which is used when this is \code{NULL}.
#' @param ... name= value parameters that will be included in exception generated
#'   and will be accessible as list elements, i.e. <exception>$<name> == <value>
#'   This will replace any data elements of the same name from base exceptions.
#'   The arguments \code{exception}, \code{base}, \code{message}, and
#'   \code{call} already have meaning and can't be re-used.
#'
#' @return A new exception class inheriting the class hierarchy of the \code{base=}
#' exception, and any of that classes data elements, but with new message and
#' call elements, and any additional data elements specified.
#'
#' @export
extendException <- function( exception,
   base= Exception(), message= NULL, call= NULL, ...
) {
   objClass <- c(exception, class(base))
   objList <- as.list(base)
   if ( ! is.null( message )) { objList$message <- message }
   if ( ! is.null(    call )) { objList$call    <- call    }
   objList <- merge.list( objList, list( ... ))  # Handles empty list(...) correctly
   structure( class= objClass, objList)
}
