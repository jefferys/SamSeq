# samHeader.R - Construct and work with the header from a *.sam file

#' Return a SamHeader object
#'
#' S3 method that returns an S3 SamHeader objet.
#'
#' @param x The object to extract or build a sam header object from. x can be a
#' character vector of unparsed header lines, or a Sam object.
#'
#' @param samSource The SamSource object to associate with a header. If
#' \code{x} is a Sam object, it will use that objects SamSource, otherwise it
#' will use a default \code{SamSource()}
#'
#' @export
SamHeader <- function(x, ...) {
	UseMethod("SamHeader", "x")
}

#' @export
SamHeader.character <- function( x, samSource= SamSource(NULL), ... ) {
	tag= substr(trimws(x), 1, 2)
	if (any(substr( tag, 2, 2 ) == ":")) {
		warning( "Possible use of one character tags in headers" )
	}
	return( structure(
		class="SamHeader", source= samSource,
		data.frame(
			tag=tag,
			record = trimws(sub("^..:", "", x)),
			row.names= 1:length(x),
			stringsAsFactors = FALSE
		)
	))
}

#' @export
SamHeader.Sam <- function( x, ... ) {
	return( structure(
		class="SamHeader", source= x$source,
		x$header
	))
}

#' @export
as.data.frame.SamHeader <- function (x, ...) {
	return(x$header)
}

