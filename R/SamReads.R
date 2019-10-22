# samReads.R - Construct and work with the reads from a *.sam file

#' Return a SamReads object
#'
#' S3 method that returns an S3 SamReads object, which is a data frame with
#' additional meta data.
#'
#' @param x For the object to extract or build a sam reads object from. It can
#'   be a character vector of unparsed read lines, a \code{Sam} object, or
#'   (pretty uselessly) a \code{SamReads} object.
#'
#' @param source The \code{SamSource} object to associate with the reads. If
#'   not provided and \code{x} is a \code{Sam} or \code{SamReads} object, it
#'   will default to that object's source. Otherwise the default is all missing
#'   data, i.e. \code{SamSource(NULL)}
#'
#' @param splitTags By default this is \code{TRUE}, meaning optional read tags
#'   are split out as named columns. Setting this \code{FALSE} leaves the
#'   optional tags as a single column, "tags", with all the extra tags
#'   presented as a single tab delimited string.
#'
#' @param ... Future-proof the method. Not currently used.
#'
#' @return A \code{SamReads} object.
#'
#' @export
SamReads <- function (x, ...) {
	UseMethod("SamReads")
}

#' Parse the optional sam read tags into a data frame
#'
#' @param tagList An un-named list with one element per read line, in order. Each read line
#' element is itself an un-named list with one element per sam record field. Each field from a
#' given column (by default 12) must be an optional sam tag formatted as key:typeFlag:value. Only these optional sam tags are parsed. If only optional tags are present, can set \code{startAt= 1}. The
#' order of optional tags as first encountedered determines the order reported.
#'
#' @param startAt The first column in the field list that is to be parsed. All
#' columns after this are assumed to be optional tag fields. By default the
#' 12'th column is the start of the optional tags; this is the default full sam record.
#'
#' @return A data frame with one column per two character parsed tag. Values are
#' converted to integer if typeFlag is i and to double if typeFlag is f.
#' Otherwise left as character. Any tag that is found only on some lines will
#' have a value of NA on any line where it is not found.
#'
#' @section Messages:
#'
#' As this takes a long time, intermediate messages are printed to STDOUT
#' while running.
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
		return( dplyr::bind_cols( data, data.frame(tags= sapply( readList, function(x) {
			paste(x[ c( 12:max(12,length( x )))], collapse="\t")
		}), stringsAsFactors = FALSE)))
	}
}

#' @rdname SamReads
#' @export
SamReads.character <- function( x, source= SamSource(NULL), splitTags= TRUE, ... ) {
	return( structure(
		class="SamReads", source= source,
		list( reads= parseSamReadLines( x, splitTags= splitTags ),
				areReadTagsParsed= splitTags )
	))
}

#' @rdname SamReads
#' @export
SamReads.Sam <- function( x, ... ) {
	return( structure(
		class= "SamReads", source= attr(x, "source"),
		list( reads= x$reads,  areReadTagsParsed= x$areReadTagsParsed)
	))
}

#' @rdname SamReads
#' @export
as.data.frame.SamReads <- function( x, ... ) {
	return( x$reads )
}

#' Read statistics
#'
#' @param x A \code{Sam} or \code{SamReads} object to return read statistics for.
#'
#' Returns the number of lines of reads in the Sam file. Does no filtering, so
#' counts a multi-aligned read end once for each alignment and includes
#' duplicates, unaligned reads, failed QC alignments, etc. Can filter and then
#' re-apply to get any sub-scount desired. Dividing by 2 does not convert to
#' paired end results as some ends may match more or less times than another.
#'
#' @param ... Required for S3 object method implementation. Not currently used.
#'
#' @return The total number of read records in the Sam file object.
#'
#' @export
countAllReads <- function(x, ...) {
	UseMethod("countAllReads")
}

#' @export
countAllReads.Sam <- function(x, ...) {
	return( countAllReads( SamReads( x )))
}

#' @export
countAllReads.SamReads <- function(x, ...) {
	return( nrow( x$reads ))
}
