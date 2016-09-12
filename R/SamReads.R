# samReads.R - Construct and work with the reads from a *.sam file

SamReads <- function (x, ...) {
	UseMethod("SamReads", x)
}

#' @export
SamReads.character <- function( lines ) {
	return( data.frame() )
}

#' @export
SamReads.Sam <- function( sam ) {
	return( structure(
		class="SamReads", source= sam$source,
		sam$reads
	))
}
