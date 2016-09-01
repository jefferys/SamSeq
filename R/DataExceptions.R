#' The DataException hierarchy
#'
#' \code{DataException}s and their sub-classes describe problems with data
#' values, names, or formats. Subclasses extended the specificity of the
#' \code{DataException}. Each takes a \code{data} argument and possibly others.
#' See \code{\link{Exception}}. These exceptions are intended to be signaled,
#' i.e. with \code{stop} and exist both to provide pre-defined error messages
#' and so that handlers in try/catch blocks have latitude to choose what
#' granularity of exception to catch.
#'
#' @param message The message associated with the exception, for reading by
#'   humans. The default message is usually good enough, but can be overridden
#'   through this parameter if needed. When constructed inside packages, the
#'   message displayed will have the name of the package prepended as
#'   \code{'[\var{package}] \var{message}'} due to \code{conditionMessage} being
#'   over-ridden for \code{Exception} subclasses.
#' @param call The call or call stack associated with the exception. By default
#'   this is NUL. Can be overridden to provide the \code{\link{Sys.calls}} call
#'   stack or a single \code{\link{Sys.call}}.
#' @param package The package where this exception is generated. May be
#'   \code{NULL} when exceptions are generated and signaled from interactive or
#'   script level code and not from a package. Attempts to guess the package by
#'   default. Will usually include the package in the displayed \code{message}.
#' @param data The data element associated with an exception. Format varies and
#'   should be described in subclass documentation. This is required for all
#'   \code{DataException}s and sub-classes, but may be \code{NULL} or \code{NA};
#'   Subclasses may restrict allowed values. Accessible via the the
#'   \code{exceptionData} S3 accessor.
#' @param path The path associated with an exception, as a one element character
#'   vector This is required for a \code{FileFormatException} or sub-class. It
#'   may be \code{NA} but should not be \code{NULL}. Accessible via the the
#'   \code{exceptionPath} S3 accessor.
#' @param line The line associated with an exception, as a one element integer
#'   vector This is required for a \code{FileFormatException} or sub-class. It
#'   may be \code{NA} but should not be \code{NULL}. Accessible via the the
#'   \code{exceptionLine} S3 accessor.
#' @param ... Additional arguments defining \code{\var{arg}= \var{value}} data
#'   to include in the Exception (accessible as list elements, but preferably
#'   by S3 accessor functions).
#'
#' @describeIn DataException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems with data values, names, or formats.
#' \itemize{
#'    \item message: "\code{[\var{package}] A DataException occurred.}"
#'    \item class: \code{c( "DataException", "Exception", "condition" )}
#'    \item data: \code{data} - see \code{exceptionData}.
#' }
#'
#' @return An \code{DataException} or descendant describing some problematic
#'   event involving a data values, names, or formats. Always is/extends
#'   class \code{c( "DataException", "Exception", "condition" )}.
#'
#' @seealso \code{condition}, \code{conditionMessage()}, \code{conditionCall},
#'   \code{Exception}, \code{exceptionPackage}, \code{exceptionData}
#'
#' @examples
#' dEx <- IOException()
#' conditionMessage(dEx)
#' conditionCall(dEx)
#' exceptionPackage(dEx)
#' exceptionData(dEx)
#' @export
DataException <- function( data, message= 'A DataException occurred.',
								 call= NULL, package=packageName(), ...
) {
	e <- Exception( message= message, call= call, package= package, data=data, ... )
	extendException( "DataException", e )
}

#' @describeIn DataException
#'
#' This exception and sub-classes are \code{Exception}s that describe
#' problems with data files
#' \itemize{
#'    \item message: "\code{[\var{package}] A FileFormatException occurred at
#'    	line \var{line} in file: "\var{file}". (Running in: "\var{getwd()}").
#'    	That line began: "\var{first 20 char}".}"
#'    \item class: \code{c( "FileFormatException", "DataException", "Exception",
#'       "condition" )}
#'    \item data: \code{data} A length one character vector giving the contents
#'       of the first offending line, possibly NA. See \code{exceptionData}.
#'    \item line: \code{data} A length one integer vector giving the line
#'       number of the first offending line - see \code{exceptionLine}.
#'    \item path: \code{path} A length one character vector giving the name
#'       of the file with a problem- see \code{exceptionPath}.
#' }
#' @export
FileFormatException <- function( path, line=NA_integer_, data= NA_character_,
	message= sprintf(
		paste0(
			'A FileFormatException occurred at line %i in file: "%s". ',
   		'(Running in: "%s").\n',
			'That line began: "%s".'
		),
		line, path, getwd(), substr(data, 1, 20)
	), call= NULL, package= packageName(), ...
) {
	if (is.null(path)) {path <- NA_character_}
	if (is.null(line)) {line <- NA_integer_}
	if (is.null(data)) {data <- NA_character_}
	dEx <- DataException( message=message, call= call, package= package,
			data=data, path=path, line=line, ... )
	extendException( "FileFormatException", dEx )
}

