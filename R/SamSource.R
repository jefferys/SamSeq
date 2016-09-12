# samSource.R - Construct and work with source data *.sam file

#' @export
SamSource <- function (x, ...) {
	UseMethod("SamSource", x)
}

#' @export
SamSource.Sam <- function ( x ) {
	return( structure(
		class="SamSource",
		attr(x, "source")
	))
}
