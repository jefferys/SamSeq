# Working with sam flags

BinaryFlagDescriptor <- function(flagDescriptions) {
	flags <- names(flagDescriptions)
	if (any(grepl("[a-z]", flags))) {
		warning( "Flag names have been converted to all upper-case." )
		flags <- toupper(flags)
	}
	# Converting to UC may have made duplicates
	if (anyDuplicated(flags)) {
		stop( "Flag names may not contain duplicates" )
	}
	# As know all UC, this check is faster
	if (any(! grepl("^[A-Z][A-Z0-9_]*", flags))) {
		stop( "Flag names must start with A-Z and contain only A-Z, 0-9, and '_'." )
	}

	return( structure( class=c('BinaryFlagDescriptor', "data.frame"),
		data.frame(
			value = 2^(0:(length( flags ) - 1)),
			description = flagDescriptions,
			row.names = flags,
			stringsAsFactors = FALSE
		)
	))
}

samFlagDescriptions= c(
	READ_PAIRED= "Template has multiple segments as sequenced (two for paired end sequencing).",
	PROPER_PAIR= "All (both) tempate segments are properly aligned, according to the aligner.",
	READ_UNMAPPED= "This segment (this end) of the template is unmapped.",
	MATE_UNMAPPED= "Next segment (other end) in the template is unmapped.",
	READ_REVERSE_STRAND= "SEQ of this seqment (this end) is reverse complemented.",
	MATE_REVERSE_STRAND= "SEQ of the next segment (other end) in the template is reverse complemented.",
	FIRST_OF_PAIR= "This is the first segment in the template (first of two ends).",
	SECOND_OF_PAIR= "This is the last segment in the template (second of two ends).",
	NOT_PRIMARY_ALIGNMENT= "This is a secondary alignment for this segment (end)",
	READ_FAILS_VENDOR_QC= "Fails (some) quality control measure.",
	DUPLICATE_READ= "Is a PCR or optical duplicate.",
	SUPPLEMENTARY_ALIGNMENT= "This is a supplementary alignment for this segment (end)."
)

samFlagDescriptor <- BinaryFlagDescriptor( samFlagDescriptions )

#' Work with the sam flags field
#'
#' A sam flag field represents 12 true or false values as a 12 bit binary
#' number. This is encoded as the integer equivalent of the binary. The
#' \code{\link{samFlags}} function supports working with this, returning a named
#' logical vector decoding a provided int value, or returning the encoded int
#' value if given a logical vector or a vector of characters that correspond to flag
#' names. A data frame of names, equivalent integer values, and a short description
#' can be obtained by calling \code{\link{samFlags}} without a value.
#'
#' @section Flag descriptions:
#' The data frame returned by calling \code{\link{samFlags}} without a value has
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
#' samFlags()
#' #>         flag value                                            description
#' #> 1 readPaired     1        Template having multiple segments in sequencing
#' #> 2 properPair     2 Each segment properly aligned according to the aligner
#' #> ...
#'
#' # Encode sam flags to integer value
#' samFlags( "readPaired" )
#' #> 1
#' samFlags( c("readPaired", "readUnmapped" ))
#' #> 5
#' samFlags( "readPaired", "readUnmapped" )
#' #> 5
#'
#' samFlags( 3 )
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
samFlags <- function( x, ... ) {
	UseMethod( "samFlags")
}

#' @rdname samFlags
#' @export
samFlags.NULL <- function( x, ... ) {
	return( samFlagDescriptor )
}

#' @rdname samFlags
#' @export
samFlags.character <- function( x, ... ) {
	return( sum( samFlagDescriptor[ c(x, ...), "value"] ))
}

#' @rdname samFlags
#' @export
samFlags.numeric <- function( x, ... ) {
	return( rev( stats::setNames(
		as.logical( bitwAnd(x, samFlagDescriptor$value)),
		rownames(samFlagDescriptor)
	)))
}

#' Test if sam flags are set or unset.
#'
#' Logically compares one or more sam flags against an encoded sam value.
#'
#' Note: \code{allSetSamFlags} == \code{! anyUnsetSamFlags}, and
#' \code{allUnsetSamFlags} == \code{! anySetSamFlags}
#'
#' @param val The encoded sam flag value to check, as a single integer value
#'
#' @param flags The flags to test, as a character vector.
#'
#' @param flagVec A logical vector where the names are sam flags and the values
#'   indicate if that flag shoud be set (\code{TRUE}) or unset (\code{FALSE}). If
#'   the value of a flag does not matter, it should not be included. NA should
#'   not be used.
#'
#' @return A single TRUE or FALSE value
#'
#' @examples
#' # READ_PAIRED = 1, PROPER_PAIR = 2
#' # All TRUE
#' allSetSamFlags( 1 + 2, c( "READ_PAIRED", "PROPER_PAIR" ))
#' anySetSamFlags( 1 + 2, c( "READ_PAIRED", "PROPER_PAIR" ))
#' anySetSamFlags( 1 + 2, "READ_PAIRED")
#' allUnsetSamFlags( 4, c( "READ_PAIRED", "PROPER_PAIR" ))
#' anyUnsetSamFlags( 1, "PROPER_PAIR" )
#' anyUnsetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" ))
#'
#' # All FALSE
#' allSetSamFlags( 1, "PROPER_PAIR" )
#' allSetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" ))
#' anySetSamFlags( 4, c( "READ_PAIRED", "PROPER_PAIR" ))
#' allUnsetSamFlags( 1 + 2, c( "READ_PAIRED", "PROPER_PAIR" ))
#' allUnsetSamFlags( 1 + 2, "READ_PAIRED" )
#' anyUnsetSamFlags( 1 + 2, c( "READ_PAIRED", "PROPER_PAIR" ))
#'
#' goodAlignmentFlagVec <- c(
#' 	"READ_PAIRED"           =  TRUE, "PROPER_PAIR"             =  TRUE,
#'    "READ_UNMAPPED"         = FALSE, "MATE_UNMAPPED"           = FALSE,
#'    "NOT_PRIMARY_ALIGNMENT" = FALSE, "READ_FAILS_VENDOR_QC"    = FALSE,
#'    "DUPLICATE_READ"        = FALSE, "SUPPLEMENTARY_ALIGNMENT" = FALSE
#' )
#' matchSamFlags( 1+2, goodAlignmentFlagVec )    # TRUE
#' matchSamFlags( 1+2+4, goodAlignmentFlagVec )  # FALSE
#' matchSamFlags( 1, goodAlignmentFlagVec )  # FALSE
#' @name testSamFlags
NULL
## NULL

#' @describeIn testSamFlags TRUE only if all specified flags are set, FALSE if
#'   any specified flag is unset. The status of unspecified flags does not
#'   matter.
#'
#' @export
allSetSamFlags <- function(val, flags) {
	all(bitwAnd(val, samFlagDescriptor[ flags, "value"]))
}

#' @describeIn testSamFlags TRUE only if all specified flags are unset, FALSE if
#'   any specified flags is set. The status of unspecified flags does not
#'   matter.
#'
#' @export
allUnsetSamFlags <- function(val, flags) {
	! any(bitwAnd(val, samFlagDescriptor[ flags, "value"]))
}

#' @describeIn testSamFlags TRUE if any specified flag is set, FALSE only if all
#'   specified flags are unset. The status of unspecified flags does not matter.
#'
#' @export
anySetSamFlags <- function(val, flags) {
	any(bitwAnd(val, samFlagDescriptor[ flags, "value"]))
}

#' @describeIn testSamFlags TRUE if any specified flag is unset, FALSE only if
#'   all specified flags are set. The status of unspecified flags does not
#'   matter.
#'
#' @export
anyUnsetSamFlags <- function(val, flags) {
	! all(bitwAnd(val, samFlagDescriptor[ flags, "value"]))
}

#' @describeIn testSamFlags TRUE if all flags specified as TRUE are set and all
#' flags specified as FALSE are unset. The status of unspecified flags does not
#' matter.
#'
#' @export
matchSamFlags <- function(val, flagVec) {
	  all(bitwAnd(val, samFlagDescriptor[ names(flagVec)[flagVec], "value"])) &&
	! any(bitwAnd(val, samFlagDescriptor[ names(flagVec)[! flagVec], "value"]))
}
