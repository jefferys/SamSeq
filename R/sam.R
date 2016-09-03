
# STUB
loadSamHeader <- function( file ) {
	return(list())
}

#' Create a sam (sequence alignment/map) object.
#'
#' An object of class \code{Sam} represents a \file{*.sam} file. This is
#' supposed to be an opaque object for use by other functions. The size of a
#' \code{sam} object is limited to only what can be read into memory and
#' manipulated, which is probably at best about 1/3 of the total memory on the
#' system (due to \R functions being "copy by value".) No
#' support is provided for \file{*.bam} files.
#'
#' A sam object is composed of three parts, a \code{\link{SamHeader}},
#' \code{\link{SamReads}}, and \code{\link{SamMeta}} data. These parts can be
#' extracted as objects of the same name by constructor functions of the same
#' name applied to the \code{Sam} object. Any function that works when applied
#' to a component object will work on the \code{Sam} object too.
#'
#' @param file A *.sam file name.
#'
#' @return An object of class Sam.
#'
#' @examples
#' samObj <- sam( 'myFile.sam' )
#'
#' myHeader <- samHeader(samObj)
#' myReads <- samReads(samObj)
#' myMeta <- samMeta(samObj)
#' @export
Sam <- function( file ) {

	# Open sam file for reading
   if (! file.exists(file)) {
      stop( NoSuchFileException( file ))
   }

	# Ensure we have header lines
	firstLine <- readLines(file, n= 1L)
	if (length(firstLine) < 1L) {
		# If have just an empty line as first line, still read in vector
		# of length 1.
		stop( EmptyFileException( path= file ))
	}

	if (! grepl( "^@..\t", firstLine )) {
		# Doesn't allow empty lines before header lines either.
		stop( MISSING_HEADER_Exception(path= file))
	}

	# Throws Exception ensures we have read lines, as don't know we have all headers
	# until hit end of file or a read line. Reads can not be named "@**".
	headers <- loadSamHeader( file )
	offset <- length(headers)


	# maxHeaderSize <- 1e5;
	# gotHeaders <- FALSE;
	# while (! gotHeaders) {
	# 	containsAllHeaders <- readLines(sam_fh, n= maxHeaderSize)
	# 	lastLine <- containsAllHeaders[length(containsAllHeaders)]
	# 	if (nchar(lastLine) < 1 || ! grepl( "^@..\t", lastLine )) {
	# 		gotHeaders <- TRUE
	# 	}
	# 	else if (length(containsAllHeaders) < maxHeaderSize) {
	# 		warning( NoReadsSamException() )
	# 	}
	# 	else {
	# 		maxHeaderSize <- maxHeaderSize * 10
	# 	}
	# }

	sam <- list(header=list(), reads=list(), meta=list(file=file))
   return(structure(class="Sam", sam))
}


#' @export
SamHeader <- function( sam ) {
	header <- structure( class="SamHeader", sam$header )
	return(header)
}

#' @export
SamReads <- function( sam ) {
	reads <- structure( class="SamReads", sam$reads )
	return(reads)
}

#' @export
SamMeta <- function( sam ) {
	meta <- structure( class="SamMeta", sam$meta )
	return(meta)
}
