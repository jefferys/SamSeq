# samHeader.R - Construct and work with the header from a *.sam file

#' Return a SamHeader object
#'
#' S3 method that returns an S3 SamHeader objet.
#'
#' @param x The object to extract or build a sam header object from. x can be a
#' character vector of unparsed header lines, or a Sam object.
#'
#' @param source The SamSource object to associate with a header. If
#' \code{x} is a Sam object, it will use that objects SamSource, otherwise it
#' will use a default \code{SamSource()}
#'
#' @export
SamHeader <- function(x, ...) {
	UseMethod("SamHeader")
}

#' Parse strings as sam file header lines
#'
#' @param x Vector of string, each a sam file header
#'
#' @return A data frame with two columns, the first giving the tag name, the
#' second the original full line. Not the final version of this :)
#'
#' @export
parseSamHeaderLines <- function( x ) {
	tag= substr(trimws(x), 2, 3)
	if (any(substr( x, 1, 1 ) != "@")) {
		stop( "Header lines must begin with \"@\"" )
	}
	if (any(substr( tag, 2, 2 ) == ":")) {
		warning( "Possible use of one character tags in headers" )
	}
	data.frame(
		tag=tag,
		record = trimws(sub("^@..:", "", x)),
		row.names= 1:length(x),
		stringsAsFactors = FALSE
	)
}

#' @export
SamHeader.character <- function( x, source= SamSource(NULL), ... ) {
	return( structure(
		class= c("SamHeader", "data.frame"), source= source,
		.Data = parseSamHeaderLines(x)
	))
}

#' @export
SamHeader.Sam <- function( x, ... ) {

	return( structure(
		class= "SamHeader", source= attr(x, "source"),
		x$header
	))
}
