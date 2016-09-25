
#' Create a sam (sequence alignment/map) object.
#'
#' An object of class \code{Sam} represents a \file{*.sam} file. This is
#' supposed to be an opaque object for use by other functions. The size of a
#' \code{sam} object is limited to only what can be read into memory and
#' manipulated, which is probably at best about 1/3 of the total memory on the
#' system (due to \R functions being "copy by value".) No support is provided
#' for \file{*.bam} files.
#'
#' A sam object is composed of three parts, a \code{\link{SamHeader}},
#' \code{\link{SamReads}}, and \code{\link{SamSource}} data. These parts can
#' be extracted as objects of the same name by constructor functions of the same
#' name applied to the \code{Sam} object. Any function that works when applied
#' to a component object will work on the \code{Sam} object too.
#'
#' @param file A *.sam file name.
#'
#' @param splitTags Parse optional sam file read tags into columns. True by
#'   default, but increases processing time a factor of 4 or so. If false will
#'   have only one "tags" column with a single tab-delimited string.
#'
#' @return An object of class Sam.
#'
#' @examples
#' samObj <- sam( 'myFile.sam' )
#'
#' myHeader <- samHeader(samObj)
#' myReads <- samReads(samObj)
#' mySource <- samSource(samObj)
#' @export
Sam <- function( file, splitTags= TRUE ) {

	# Validate - File exists
   if (! file.exists(file)) {
      stopWith( NoSuchFileException( file ))
   }

	# Optimization note - File will be very large. Reading all into memory
	# first may be too memory expensive.
	print( strwrap('Reading file: "' %p% file %p% '". This may take a few seconds.',
						exdent = 4 ))
	data <- readLines(file)

	if (length(data) < 1L) {
		stopWith( EmptyFileException( path= file ))
	}


	# Find the indices of all lines that don't start with a header record symbol
	# and keeps the first such index. This is the index of the first data line.
	# Note when all checked elements are FALSE, which() returns an empty vector.
	# The first index of that is NA, so if there are only header lines,
	# readStartLine will be, reasonable, NA.
	readStartLine <- which(! grepl( "^@..\t", data ))[1]

	# Validate - Has at least 1 non-header line, aassumed to be a read line.
	if ( is.na( readStartLine )) {
		stopWith( HeaderOnlyException( path= file ))
	}

	# Validate - Has at least 1 header line, aassumed to be a read line.
	if ( readStartLine == 1L ) {
		stopWith( MISSING_HEADER_Exception( path= file ))
	}

	header <- parseSamHeaderLines( data[1:(readStartLine - 1)] )
	reads <- parseSamReadLines( data[readStartLine:length(data)], splitTags= splitTags )
	source <- SamSource( file, host= Sys.info()["nodename"], type="file" )

   return( structure( class= c("Sam", "list"), source= source,
   						 .Data=list(header=header, reads=reads) ))
}

