#' Filter paired or unpaired reads
#'
#' Drop all paired or unpaired reads. Relies on the accuracy and disjoint nature
#' of the FIRST_OF_PAIR and SECOND_OF_PAIR flags, and that each paired-end read
#' has a unique id (qname). Maintains multiple alignments for any kept read
#' ends.
#'
#' This allows dropping unpaired reads after filtering that might only drop one
#' read of a pair, such a filtering by quality.
#'
#' @param sam The Sam object to filter.
#'
#' @param paired Set \code{FALSE} to keep only unpaired reads. Keeps
#' only paired reads by default.
#'
#' @return A sam object with only paired reads, or with only unpaired reads.
#'
#' @examples
#' \dontrun{
#' sam <- Sam("someFile")
#' pairedOnly <- pairedReads(sam)
#' unpairedOnly <- pairedReads(sam, FALSE)
#' }
#'
#' @export
pairedReads <- function(sam, paired= TRUE) {
	# as.logical(bitwAnd(sam$reads$flag, 64)) is true for all first reads in pair
	#  (for each alignment if more than one). 128 is true for second.
	if (paired) {
		sam$reads <- sam$reads[
			sam$reads$qname %in% sam$reads$qname[as.logical(bitwAnd(sam$reads$flag, 64))] &
			sam$reads$qname %in% sam$reads$qname[as.logical(bitwAnd(sam$reads$flag, 128))], ]
	}
	else {
		sam$reads <- sam$reads[
			! sam$reads$qname %in% sam$reads$qname[as.logical(bitwAnd(sam$reads$flag,  64))] |
			! sam$reads$qname %in% sam$reads$qname[as.logical(bitwAnd(sam$reads$flag, 128))], ]
	}
	sam
}

#' Filter a Sam object
#'
#' Apply a filter to a sam object, returing a new sam object with the sam header
#' but only containing the reads for which the filter \code{FUNC} returns
#' \code{TRUE}. Uses \code{\link[plyr]{adply}} to apply this filter to each read
#' alignment. Many of the parameters are just pass-through parameters for
#' \code{\link[plyr]{adply}}. These all begin with a "\code{.}" to distinguish
#' them from potential parameters to the function as they come after the
#' \code{...} in the function definition. No attempt is made to keep or delete
#' both ends of a paired end read if one end passes and one end fails the
#' filter.
#'
#' NOTE: Does not add a @PG header line nor modify any PG:Z tag on any read.
#'
#' @param sam A Sam object
#' @param FUNC A function whose first parameter is a single read and that
#'   returns a single \code{TRUE} if that read should be kept when filtering the
#'   \code{sam} object, or \code{FALSE} if not. Should never return \code{NULL}
#'   or \code{NA}.
#' @param ... Other parameters to pass to \code{FUNC}.
#' @param .progress Progress bar; see \code{\link[plyr]{adply}}.
#' @param .inform Extra error handling; see \code{\link[plyr]{adply}}.
#' @param .parallel Use the parallel package; see \code{\link[plyr]{adply}}.
#' @param .paropts Options if using the parallel package; see \code{\link[plyr]{adply}}.
#' @param .id Id column; see \code{\link[plyr]{adply}}.
#'
#' @return A sam object with only the reads that passed the filter \code{FUNC}.
#'
#' @seealso Sam, readIsChimeric,
#'
#' @examples
#' \dontrun{
#' sam <- Sam("someFile")
#' hivIntegrationSam <- samReadFilter( sam, "readIsOnRef", "^chrMT$" )
#' }
#'
#' @export
samReadFilter <- function( sam, FUNC, ...,
	.progress= "none", .inform= FALSE, .parallel= FALSE, .paropts= NULL, .id= NA
) {
	sam$reads <- sam$reads[plyr::aaply(
		.data= sam$reads, .margins= 1, .fun= FUNC, ..., .expand= FALSE,
		.progress= .progress, .inform= .inform, .parallel= .parallel,
		.paropts= .paropts
	), ]
	sam
}

#' Paired-end read tests
#'
#' Tests a read to see if its meets some criteria. Only
#' tests that keep both or delete both reads when applied to each read end
#' separately are paired-end read tests. These tests are suitable for use in a
#' \code{\link{samReadFilter}} and will not result in unpaired reads after
#' application.
#'
#' @param read The read to test.
#'
#' @param ref Regular expression matched against the reference this and/or the
#'   other end aligns too. Keeps a read if \code{ref} matches to either
#'   (\code{rname}) or (\code{rnext}) or both.
#' @param ref1 Regular expression that matches the chromosome/reference name
#'   of one end, either \code{rname} or \code{rnext}. Should not match both.
#' @param ref2 Reqular expression that matches the chromosome/reference name of
#'   whichever end \code{ref1} does not match.
#'
#' @return Returns \code{TRUE} if a read passes the test, \code{FALSE}
#'   otherwise. Each read from the same template will return the same result.
#'
#' @examples
#' \dontrun{
#' sam <- Sam("someFile")
#' aRead <- SamReads(sam)[1]
#'
#' pairHasRef( aRead, "^chrX$|^chrY$" )
#' sexLinked <- samReadFilter( sam, "pairHasRef", "^chrX$|^chrY$" )
#'
#' pairIsChimeric(aRead, "^chr..?$", "hiv|HIV")
#' hivIntegrationSam <- samReadFilter( sam, "pairIsChimeric", "^chr..?$", "hiv|HIV" )
#'
#' pairIsNotChimeric(aRead, "hiv|HIV")
#' hivOnly <- samReadFilter( sam, "pairIsNotChimeric", "hiv|HIV" )
#'
#' }
#' @name pairedEndReadTests
NULL
#NULL

#' @describeIn pairedEndReadTests selects paired end reads when both ends
#' are aligned. Relies on accuracy of READ_UNMAPPED and MATE_UNMAPPED flags and
#' does not consider any other indicators. This may select multiple reads for one
#' or both ends.
#'
#' @export
pairIsAligned <- function(read) {
	allUnsetSamFlags(read$flag, c("READ_UNMAPPED", "MATE_UNMAPPED"))
}

#' @describeIn pairedEndReadTests selects paired end reads when both ends
#' are aligned and are the primary alignment. Relies on accuracy of the
#' READ_UNMAPPED, MATE_UNMAPPED, and NOT_PRIMARY_ALIGNMENT flags.
#'
#' @export
pairIsPrimary <- function(read) {
	allUnsetSamFlags(read$flag, c("READ_UNMAPPED", "MATE_UNMAPPED", "NOT_PRIMARY_ALIGNMENT"))
}

#' @describeIn pairedEndReadTests Identifies paired end reads when one end is aligned
#'   to a selected reference or chromosome. Takes a regular expression and
#'   will return TRUE if either a reads \code{rnext} or \code{rname} (or both)
#'   matches it.
#'
#' @export
pairHasRef <- function(read, ref ) {
	return( grepl( ref, read$rname ) || grepl( ref, read$rnext ))
}

#' @describeIn pairedEndReadTests Identifies paired end reads when one end is
#'   aligned to one reference or chromosome and the other end to a different
#'   one. Takes two regular expressions, and will return TRUE only if one read
#'   end matches the first expression and the other read end matches the second.
#'
#' @export
pairIsChimeric <- function(read, ref1, ref2 ) {
	(grepl( ref1, read$rname ) && grepl( ref2, read$rnext )) ||
	(grepl( ref2, read$rname ) && grepl( ref1, read$rnext ))
}

#' @describeIn pairedEndReadTests Identifies paired end reads when both ends are
#'   aligned to the same reference or chromosome and the other end to a different
#'   one. Takes a regular expressiona, and will return TRUE only if both a reads
#'   \code{rnext} and \code{rname} match it.
#'
#' @export
pairIsNotChimeric <- function(read, ref) {
	return( grepl( ref, read$rname ) && grepl( ref, read$rnext ))
}

#' Read tests
#'
#' Tests a read to see if its meets some criteria. May return \code{TRUE} for
#' only one end of a paired end read. These tests are suitable for use in a
#' \code{\link{samReadFilter}} but can result in unpaired reads after
#' application; \code{pairedReads} should probably be run after running any
#' of these as filters.
#'
#' @param read The read to test.
#' @param ref Regular expression matched against the reference aligned to.
#' Keeps a read if \code{ref} matches to the reference this read aligns to
#' (\code{readHasRef()}, matching against \code{rname}) or the reference the
#' mate of this read aligns to (\code{readMateHasRef()}, matching against
#' \code{rnext}).
#' @param flags A character vector of one or more flag names to test against
#' the flags set or unset for a read.
#' @param flagVec A logical vector where the names are sam flags and the values
#' indicate if that flag shoud be set (\code{TRUE}) or unset (\code{FLAG}). If
#' the value of a flag does not matter, it should not be included. NA should not
#' be used.
#'
#' @return Returns \code{TRUE} if a read passes the test, \code{FALSE} otherwise.
#' Different ends of the same template will return the same result.
#'
#' @examples
#' \dontrun{
#' sam <- Sam("someFile")
#' aRead <- SamReads(sam)[1]
#'
#' areReadFlagsUnset( aRead, "NOT_PRIMARY_ALIGNMENT" )
#' primaryReads <- samReadFilter( sam, "areReadFlagsUnset", "NOT_PRIMARY_ALIGNMENT" )
#'
#' areReadFlagsSet( aRead, "NOT_PRIMARY_ALIGNMENT" )
#' secondaryReads <- samReadFilter( sam, "areReadFlagsSet", "NOT_PRIMARY_ALIGNMENT" )
#'
#' goodAlignmentFlagVec <- c(
#' 	"READ_PAIRED"           =  TRUE, "PROPER_PAIR"             =  TRUE,
#'    "READ_UNMAPPED"         = FALSE, "MATE_UNMAPPED"           = FALSE,
#'    "NOT_PRIMARY_ALIGNMENT" = FALSE, "READ_FAILS_VENDOR_QC"    = FALSE,
#'    "DUPLICATE_READ"        = FALSE, "SUPPLEMENTARY_ALIGNMENT" = FALSE
#' )
#' areReadFlags( aRead, goodAlignmentFlagVec )
#' goodReads <- samReadFilter( sam, "areReadFlags", goodAlignmentFlagVec )
#' }
#' @name readTests
NULL
#NULL

#' @describeIn readTests Returns \code{TRUE} if all the specified flags are set for
#' the read. Equivalent to \code{\link{allSetSamFlags}(read$flag, flags)}
#'
#' @export
areReadFlagsSet <- function(read, flags) {
	allSetSamFlags(read$flag, flags)
}

#' @describeIn readTests Returns \code{TRUE} if all the specified flags are set for
#' the read. Equivalent to \code{\link{allUnsetSamFlags}(read$flag, flags)}
#'
#' @export
areReadFlagsUnset <- function(read, flags) {
	allUnsetSamFlags(read$flag, flags)
}

#' @describeIn readTests Returns \code{TRUE} if all the flags that set in the
#'   flagVec are set for the read and all the flags that are unset in the
#'   flagVec are unset in the read. If all flags in the flagVec checked are set
#'   or all are unset, it is more efficient to use \code{areReadFlagsSet} or
#'   \code{areReadFlagsUnset}.
#'
#' @export
areReadFlags <- function( read, flagVec ) {
	matchSamFlags( read$flag, flagVec )
}

#' @describeIn readTests Returns \code{TRUE} if the read is aligned to a
#'   reference identified by the specified regular expressions (matched against
#'   the reads \code{rname}).
#'
#' @export
readHasRef <- function( read, ref ) {
	grepl( ref, read$rname )
}

#' @describeIn readTests Returns \code{TRUE} if the read's mate (other paired
#'   end) is aligned to a  reference identified by the specified regular
#'   expressions (matched against the reads \code{rnext}).
#'
#' @export
readMateHasRef <- function( read, ref ) {
	grepl( ref, read$rnext )
}
