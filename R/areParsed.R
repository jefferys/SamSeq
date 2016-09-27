#' Are sam tags parsed
#'
#' Accessors for checking if the data associated with this Sam file has parsed
#' tags, or has one column with tags unparsed. Both reads and headers will have
#' a parsed and an unparsed version, although currently only an unparsed version
#' is avaialble for headers.
#'
#' @param x The Sam, SamReads or SamHeader object to check for parsed tags.
#' @param ... Required for S3 object method implementation. Not currently used.
#'
#' @return TRUE if tags are parsed, FALSE otherwise.
#'
#' @name areTagsParsed
NULL
# NULL

#' @rdname areTagsParsed
#' @export
areReadTagsParsed <- function( x, ... ) {
	UseMethod("areReadTagsParsed")
}

#' @rdname areTagsParsed
#' @export
areReadTagsParsed.SamReads <- function( x, ... ) {
	return( x$areReadTagsParsed )
}

#' @rdname areTagsParsed
#' @export
areReadTagsParsed.Sam <- function( x, ... ) {
	return( areReadTagsParsed(SamReads(x)) )
}

#' @rdname areTagsParsed
#' @export
areHeaderTagsParsed <- function( x, ... ) {
	UseMethod("areHeaderTagsParsed")
}

#' @rdname areTagsParsed
#' @export
areHeaderTagsParsed.SamHeader <- function( x, ... ) {
	return( x$areHeaderTagsParsed )
}

#' @rdname areTagsParsed
#' @export
areHeaderTagsParsed.Sam <- function( x, ... ) {
	return( areHeaderTagsParsed(SamHeader(x)) )
}
