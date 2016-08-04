#' Loads a *.sam file.
#'
#' Reads the data from a \file{*.sam} file into a \code{samFile} object. This is
#' supposed to be an opaque object for use in other functions. Limited to only
#' what can be read into memory and manipulated, which is probably at best about
#' 1/3 of the total memory on the system (due to \R functions being "copy by
#' value".) Does not support reading \file{*.bam} files.
#'
#' @seealso \code{samHeader}, \code{samReads}, \code{samMeta}.
#'
#' @section Internals: A list with three elements:
#' \itemize{
#'    \item{"samHeader" - a \code{samHeader} object.}
#'    \item{"samReads" - a \code{samReads} object.}
#'    \item{"samMeta" - a \code{samMeta} object.}
#' }
#'
#' @return A \code{samFile} object.
#'
#' @examples
#' mySamFile <- samFile( 'myFile.sam' )
#'
#' @param file The name of the sam file to load.
#' @export
samFile <- function( file ) {
   structure(list(), class="samFile")
}
