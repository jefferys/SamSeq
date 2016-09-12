# samHeader.R - Construct and work with the header from a *.sam file

SamHeader <- function (x, ...) {
	UseMethod("SamHeader", x)
}

#' @export
SamHeader.character <- function( lines ) {
	return( data.frame() )
}

#' @export
SamHeader.Sam <- function( sam ) {
	return( structure(
		class="SamHeader", source= sam$source,
		sam$header
	))
}

