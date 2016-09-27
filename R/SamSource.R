# samSource.R - Construct and work with source data *.sam file

#' Create or retrieve \code{SamSource} objects.
#'
#' A \code{SamSource} object is meant to be an opaque object used to store the
#' source information associated with a Sam file. Currently only \code{type=
#' "file"} is supported, but \code{"url"} may be added. The name, host machine
#' name, and type can be retrieved from \code{SamSource} or \code{Sam} objects
#' with \code{samSourceName()}, \code{samSourceHost()}, and
#' \code{samSourceType()}, respectively. A \code{SamSource} object can be
#' retrieved from a \code{Sam}, \code{SamHeader}, or \code{SamReads} object
#' using \code{SamSource()}.

#' @param x The object to construct a SamSource object from. Can be a Sam,
#'   SamHeader, or SamReads object, a list, or the name of the source file/url
#'   as a character vector, or an explicit NULL. If this is a list ,
#'   \code{"name="}, \code{"host="} and \code{"type="} elements can be specified
#'   as character vectors. Missing or NULL list elements are treated as N/A. All
#'   character vectors should be the same length. If this is an explicit
#'   \code{NULL}, it is treated the same as if it was an empty list.
#' @param host The name of the system the sam file is available on. Default is
#'   \code{NA}.
#' @param type The type of source for the sam file. Currently only "file" is
#'   supported. "url" may be added. The default is NA.
#' @param ... Required for S3 object method implementation. Not currently used.
#'
#' @return Returns a SamSource object, an opaque object from which the source
#'   file name, host machine, and type of access can be extracted.
#'
#' @examples
#' \dontrun{
#'   sam <- Sam("file.sam")
#'   stopIfNot( SamSource( sam ) == SamSource( SamHeader( sam ) ==
#'   	SamSource( SamReads( sam )))
#'
#'   source2 <- SamSource( "file.sam", host="bioinf.unc.edu", type="file")
#'
#'   sourceList <- list(name="file.sam", host="bioinf.unc.edu", type="file")
#'   source3 <- SamSource( sourceList )
#'
#'   source4 <- SamSource(NULL)
#' }
#' @export
SamSource <- function (x, ...) {
	UseMethod("SamSource")
}

#' @rdname SamSource
#' @export
SamSource.character <- function ( x, host=NA, type=NA, ... ) {
	return( structure(
		class=c("SamSource", "list"),
		list( name=x, host=host, type=type )
	))
}

#' @rdname SamSource
#' @export
SamSource.list <- function ( x, ... ) {
	return( structure(
		class=c("SamSource", "list"),
		list( name=x$name, host=x$host, type=x$type )
	))
}

#' @rdname SamSource
#' @export
SamSource.Sam <- function ( x, ... ) {
	return( attr(x, "source") )
}

#' @rdname SamSource
#' @export
SamSource.SamSource <- function ( x, ... ) {
	return( x )
}

#' @rdname SamSource
#' @export
SamSource.SamHeader <- function ( x, ... ) {
	return( attr(x, "source") )
}

#' @rdname SamSource
#' @export
SamSource.SamReads <- function ( x, ... ) {
	return( attr(x, "source") )
}

#' @rdname SamSource
#' @export
SamSource.NULL <- function ( x, ... ) {
	return( structure(
		class=c("SamSource", "list"),
		list( name=NA, host=NA, type=NA )
	))
}

#' SamSource name data accessor
#'
#' Accessor to extract the name of the sam file data was taken from.
#'
#' @param x Any object for which \code{SamSource(x)} is defined.
#'
#' @return The name of the source sam data file as a character vector.
#'
#' @export
samSourceName <- function (x) {
	return( SamSource(x)$name )
}

#' SamSource host data accessor
#'
#' Accessor to extract the name of the host machine where the sam file the data
#' was taken from resides.
#'
#' @param x Any object for which \code{SamSource(x)} is defined.
#'
#' @return The name name of the host machine where the sam file the data was
#'   taken from resides as a character vector.
#'
#' @export
samSourceHost <- function (x) {
	return( SamSource(x)$host )
}

#' SamSource data type accessor
#'
#' Accessor to extract the type of access required for the sam file the data was
#' taken from resides.
#'
#' @param x Any object for which \code{SamSource(x)} is defined.
#'
#' @return The type of access required for the sam file the data was taken from,
#'   as a character vector. Can only be "file" currently. "url" is a likely
#'   future extension.
#'
#' @export
samSourceType <- function (x) {
	return( SamSource(x)$type )
}
