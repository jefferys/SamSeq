# samReads.R - Construct and work with the reads from a *.sam file

#' Return a SamReads object
#'
#' S3 method that returns an S3 SamReads object, which is a data frame with
#' additional meta data.
#'
#' @param x The object to extract or build a sam reads object from. \code{x} can
#'   be a character vector of unparsed read lines, or a \code{Sam} object.
#'
#' @param source The SamSource object to associate with the reads. If not
#'   provided and \code{x} is a \code{Sam} object, it will just use its source.
#'   For other methods, the default is all missing data, i.e.
#'   \code{SamSource(NULL)}
#'
#' @param splitTags By default this is \code{TRUE}, meaning optional read tags are
#' split out as named columns. Setting this \code{FALSE} leaves the optional tags as a
#' single column, \code{tags}, with all optional tags presented as a single tab
#' delimited string.
#'
#' @param ... Required for S3 object method implementation. Not currently used.
#'
#' @return A SamReads object, which can be used as a data frame.
#'
#' @export
SamReads <- function (x, ...) {
	UseMethod("SamReads")
}

#' Parse the optional sam read tags into a data frame
#'
#' @param tagList An un-named list with one element per read line, in order, where each
#' element is an un-named list, one element per field, where each field after a
#' given column (by default 12) is a key:typeFlag:value optional sam tag. Order
#' of the tags in the list is unimportant (although it does affect the order
#' of the tags in the returned data frame).
#'
#' @param startAt The first column in the field list that is to be parsed. All
#' columns after this are assumed to be optional tag fields. By default the
#' 12'th column is the start of the optional tags.
#'
#' @return A data frame with one column per two character parsed tag. Values are
#' converted to integer if typeFlag is i and to double if typeFlag is f.
#' Otherwise left as character. Any tag that is found only on some lines will
#' have a value of NA on any line where it is not found.
#'
parseSamReadTags <- function( tagList, startAt= 12 ) {

	print( 'Converting optional tags to named list ...' )
	namedTagLists <- lapply(tagList, function(x) {
		x <- x[ c( startAt:max(startAt,length( x )))]
		stats::setNames(
			as.list(sub("^..:[AifZHB]?:?\\s*(.*)\\s*", "\\1", x)),
			sub("^\\s*(..):([AifZHB]?).*", "\\1\\.\\2", x)
		)
	})

	print( 'Binding optional tags as data frame ...' )
	tagDF <- as.data.frame(data.table::rbindlist(namedTagLists, fill=TRUE))

	colNames <- names(tagDF)

	print( 'Converting optional tags type ...' )
	for (colName in colNames) {
		if (grepl("\\.i$", colName)) {tagDF[,colName] <- as.integer(tagDF[,colName])}
	}
	for (colName in colNames) {
		if (grepl("\\.f$", colName)) {tagDF[,colName] <- as.double(tagDF[,colName])}
	}

	colNames <- sub("\\..$", "", colNames)
	names(tagDF) <- make.names(colNames, unique=TRUE)
	return(tagDF)
}

#' Parse Sam file read lines
#'
#' @param lines A character vector of read lines, in order, from a sam file
#'
#' @param splitTags Set \code{FALSE} to leave optional tags unprocessed as a
#' \code{tags} column of characters (with embedded tabs). By default will split
#' optional tags out as columns, but this is significantly slower (5x or more).
#'
#' @return A data frame of the parsed read lines, with required column names
#'   matching the SAM spec. Optional tags, if split out, will each have their
#'   own column of values named for the two character tag. Columns will be
#'   integer or double if appropriate, all other types are left as character.
#'
#' @export
parseSamReadLines <- function( lines, splitTags= TRUE ) {
	readList <- strsplit(lines, "\t",fixed = TRUE)
	print( 'Loading basic read fields ...' )
	data <- data.frame(
		qname= sapply(readList, `[[`, 1),
		flag= as.integer(sapply(readList, `[[`, 2)),
		rname= sapply(readList, `[[`, 3),
		pos= as.integer(sapply(readList, `[[`, 4)),
		mapq= as.integer(sapply( readList, `[[`, 5)),
		cigar= sapply(readList, `[[`, 6),
		rnext= sapply(readList, `[[`, 7),
		pnext= as.integer(sapply(readList, `[[`, 8)),
		tlen= as.integer(sapply(readList, `[[`, 9)),
		seq= sapply(readList, `[[`, 10),
		qual= sapply(readList, `[[`, 11),
		stringsAsFactors = FALSE
	)
	if (splitTags) {
		return(dplyr::bind_cols(data, parseSamReadTags( readList )))
	}
	else {
		# May have problems with readList line with no tags.
		return( dplyr::bind_cols( data, tags= sapply( readList, function(x) {
			paste(x[ c( 12:max(12,length( x )))], collapse="\t")
		})))
	}
}

#' @rdname SamReads
#' @export
SamReads.character <- function( x, source= SamSource(NULL), splitTags= TRUE, ... ) {
	return( structure(
		class=c("SamReads", "data.frame"), source= source,
		parseSamReadLines( x, splitTags= splitTags )
	))
}

#' @rdname SamReads
#' @export
SamReads.Sam <- function( x, ... ) {
	return( structure(
		class= c("SamReads", "data.frame"), source= attr(x, "source"),
		x$reads
	))
}
