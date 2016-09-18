# Working with sam flags

samFlags <- data.frame(
	value= c(
		  1,    2,    4,
		  8,   16,   32,
		 64,  128,  256,
		512, 1024, 2048
	), description= c(
		"Template has multiple segments as sequenced (two for paired end sequencing).",
		"All (both) tempate segments are properly aligned, according to the aligner.",
		"This segment (this end) of the template is unmapped.",

		"Next segment (other end) in the template is unmapped.",
		"SEQ of this seqment (this end) is reverse complemented.",
		"SEQ of the next segment (other end) in the template is reverse complemented.",

		"This is the first segment in the template (first of two ends).",
		"This is the last segment in the template (second of two ends).",
		"This is a secondary alignment for this segment (end)",

		"Fails (some) quality control measure.",
		"Is a PCR or optical duplicate.",
		"This is a supplementary alignment for this segment (end)."
	),
	row.names= c(
		"READ_PAIRED",          "PROPER_PAIR",         "READ_UNMAPPED",
		"MATE_UNMAPPED",        "READ_REVERSE_STRAND", "MATE_REVERSE_STRAND",
		"FIRST_OF_PAIR",        "SECOND_OF_PAIR",      "NOT_PRIMARY_ALIGNMENT",
		"READ_FAILS_VENDOR_QC", "DUPLICATE_READ",      "SUPPLEMENTARY_ALIGNMENT"
	),
	stringsAsFactors = FALSE
)

#' Work with the sam flags field
#'
#' A sam flag field represents 12 true or false values as a 12 bit binary
#' number. This is encoded as the integer equivalent of the binary. The
#' \code{\link{samFlag}} function supports working with this, returning a named
#' logical vector decoding a provided int value, or returning the encoded int
#' value if given a logical vector or a vector of characters that correspond to flag
#' names. A data frame of names, equivalent integer values, and a short description
#' can be obtained by calling \code{\link{samFlag}} without a value.
#'
#' @section Flag descriptions:
#' The data frame returned by calling \code{\link{samFlag}} without a value has
#' one row for each of the 12 flags with the rownames giving the flag name. The
#' two columns are: \itemize{
#' 	\item{\code{value} - The integer equivalent to the binary position of the flag,
#' 		i.e. 1,2,4,8, for the 1st, second, third, etc flag.}
#' 	\item{\code{description} - A short description of the flag.}
#' }
#'  Using names and descriptions from the \href{https://github.com/samtools/htsjdk}{htsjdk}
#' project on github, specifically the MIT licensed file
#' \href{https://github.com/samtools/htsjdk/blob/master/src/main/java/htsjdk/samtools/SAMFlag.java}{SAMFlag.java}
#'
#' @param x Either encodes this vector of flag names (character) to a flag
#'   integer, or decodes this single integer value to a named logical vector.
#'   When encoding flag names order is unimportant. Integers to be decoded
#'   should be between 0 and 4095. Values larger than 4095 work like x %% 2^12.
#'   By default if nothing is provided a data frame describing the flags is
#'   returned. Note that flag names can be provided as a single vector, or just
#'   specified as arguments - see examples.
#'
#' @param ... Required for S3 formalism and future-proofing.
#'
#' @return When encoding a vector of flag names, returns a single integer value.
#'   When decoding an integer value, returns a 12 element named logical vector
#'   with the high-bit first (2048 = 2^11 = the \code{supplementaryAlignment}
#'   flag). Without a parameter to work on, returns a data frame describing
#'   the flags
#'
#' @examples
#' # Get list of flag names, values, and descriptions
#' samFlag()
#' #>         flag value                                            description
#' #> 1 readPaired     1        Template having multiple segments in sequencing
#' #> 2 properPair     2 Each segment properly aligned according to the aligner
#' #> ...
#'
#' # Encode sam flags to integer value
#' samFlag( "readPaired" )
#' #> 1
#' samFlag( c("readPaired", "readUnmapped" ))
#' #> 5
#' samFlag( "readPaired", "readUnmapped" )
#' #> 5
#'
#' samFlag( 3 )
#' #> SUPPLEMENTARY_ALIGNMENT      DUPLICATE_READ READ_FAILS_VENDOR_QC
#' #>                   FALSE               FALSE                FALSE
#' #>   NOT_PRIMARY_ALIGNMENT      SECOND_OF_PAIR        FIRST_OF_PAIR
#' #>                   FALSE               FALSE                FALSE
#' #>     MATE_REVERSE_STRAND READ_REVERSE_STRAND        MATE_UNMAPPED
#' #>                   FALSE               FALSE                FALSE
#' #>           READ_UNMAPPED         PROPER_PAIR          READ_PAIRED
#' #>                   FALSE                TRUE                 TRUE
#'
#' @export
samFlag <- function( x, ... ) {
	UseMethod( "samFlag")
}

#' @rdname samFlag
#' @export
samFlag.NULL <- function( x, ... ) {
	return( samFlags )
}

#' @rdname samFlag
#' @export
samFlag.character <- function( x, ... ) {
	return( sum( samFlags[ c(x, ...), "value"] ))
}

#' @rdname samFlag
#' @export
samFlag.numeric <- function( x, ... ) {
	return( rev( setNames(
		as.logical( bitwAnd(x, samFlags$value)),
		rownames(samFlags)
	)))
}

#' Do sam values match selected/unselected flags
#'
#' Given a vector of sam flag values, return true if the value indicates that
#' all \code{isFlag} flags are set and that none of the \code{notFlag} flags are
#' set.
#'
#' @param flagVal A vector of sam flag integer values representin the 12 sam
#'   read flags.
#'
#' @param isFlag A vector of flag (names) that must be \code{TRUE}, by default
#'   none. Returns \code{TRUE} only if the \code{flagVal} indicates that all of
#'   these flags are set.
#'
#' @param isFlag A vector of flag names that must be \code{FALSE}, by default
#'   none.  Returns \code{TRUE} only if the \code{flagVal} indicates that none
#'   of these flags are set
#'
#' @return Returns a logical vector of the same length as \code{flagVal}. TRUE
#'   if all \code{isFlag} flags are set and no \code{notFlag} flag is set. The
#'   status of flags listed in neither \code{isFlag} nor \code{notFlag} does not
#'   matter. Note - if you specify the same flag in both lists, all
#'   \code{flagVal} are FALSE. If you specify nothing in either, all are TRUE.
#'   If you specify all flags as either TRUE or FALSE (i.e. no flag is
#'   ambiguous/unimportant), it would be better to check flagVal ==
#'   samFlag(isFlag).
#'
#' @examples
#' matchFlags( 1:16, isFlag=  c( "READ_PAIRED", "PROPER_PAIR" ),
#'                   notFlag= c( "READ_UNMAPPED" ))
#' #> [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#' #> [9] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#'
#' @export
checkSamFlag <- function( flagVal, isFlag= "", notFlag= "" ) {
	sapply( flagVal, function(x) {
		all(bitwAnd(x, samFlags$value[ isFlag, ])) &
		! any(bitwAnd(x, samFlags$value[ notFlag, ]))
	})
}
