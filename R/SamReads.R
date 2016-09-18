# samReads.R - Construct and work with the reads from a *.sam file

#' Return a SamReads object
#'
#' S3 method that returns an S3 SamReads object, which is a data frame with
#' additional meta data.
#'
#' @param x The object to extract or build a sam reads object from. \code{x} can be a
#' character vector of unparsed read lines, or a \code{Sam} object.
#'
#' @param samSource The SamSource object to associate with the reads. If not
#'   provided and \code{x} is a \code{Sam} object, it will just use its source.
#'   For other methods, the default is all missing data, i.e.
#'   \code{SamSource(NULL)}
#'
#' @return A SamReads object, which is a data frame.
#'
#' @export
SamReads <- function (x, ...) {
	UseMethod("SamReads", x)
}

#' @rdname SamReads
#' @export
SamReads.character <- function( x, source= SamSource(NULL), withTags=TRUE, ... ) {
	readList <- strsplit(x, "\t",fixed = TRUE)
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
	if (withTags) {
		print( 'Converting optional tags to named list ...' )
		namedTagLists <- lapply(readList, function(x) {
			x <- x[ c( 12:max(12,length( x )))]
			setNames(
		 		as.list(sub("^..:[AifZHB]?:?\\s*(.*)\\s*", "\\1", x)),
	#	 		sub("^\\s*(..):([AifZHB]?).*", "\\1\\.\\2", x)
		 		sub("^\\s*(..).*", "\\1", x)
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

		#	colNames <- sub("\\.f$|\\.i$|\\.A$|\\.Z", "", colNames)
		# names(tagDF) <- make.names(colNames, unique=TRUE)

		return( structure(
			class=c("SamReads", "data.frame"), source= source,
	      cbind(data, tagDF)
		))
	}
	else {
		return( structure(
			class=c("SamReads", "data.frame"), source= source,
			data
		))
	}
}

#' @rdname SamReads
#' @export
SamReads.Sam <- function( x, ... ) {
	return( structure(
		class= c("SamReads", "data.frame"), source= x$source,
		x$reads
	))
}

