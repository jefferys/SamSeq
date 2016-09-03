#' Sam format exceptions
#'
#' These are a collection of functions to create data exception objects when
#' there are problems with the format or content of sam files or sam data.
#'
#' @param message The message associated with the exception, for reading by
#'   humans. The default message is usually good enough, but can be overridden
#'   through this parameter if needed. When constructed inside packages, the
#'   message displayed will have the name of the package prepended as
#'   \code{'[\var{package}] \var{message}'} due to \code{conditionMessage} being
#'   over-ridden for \code{Exception} subclasses.
#' @param call The call or call stack associated with the exception. By default
#'   this is NULL as \code{IOException}s are usually environment problems. Can
#'   be overridden to provide the \code{\link{Sys.calls}} call stack or a single
#'   \code{\link{Sys.call}}.
#' @param package The package where this exception is generated. May be
#'   \code{NULL} when exceptions are generated and signaled from interactive or
#'   script level code and not from a package. Attempts to guess the package by
#'   default. Will usually include the package in the displayed \code{message}.
#' @param path The path associated with an exception, as a one element character
#'   vector. It may be \code{NA} but should not be \code{NULL}. Accessible via
#'   the the \code{exceptionPath} S3 accessor.
#' @param line The line number associated with an exception, as a one element
#'   integer vector. It may be \code{NA} but should not be \code{NULL}.
#'   Accessible via the the \code{exceptionLine} S3 accessor.
#' @param data The data or value associated with an exception, the format can
#'   vary. Accessible via the the \code{exceptionData} S3 accessor.
#' @param ... Extra parameters for additional key=value data as desired.
#'
#' @return An \code{Exception} object whose class hierarchy is set by the
#'   function call and that may contain additional data.
#'
#' @section Htsjdk Exceptions: Names in all capitals are consistent with Htsjdk
#'   Exceptions (A Java API for high-throughput sequencing data (HTS) formats,
#'   \url{ https://github.com/samtools/htsjdk}, specifically the MIT licensed
#'   file \file{src/main/java/htsjdk/samtools/SAMValidationError.java}). These
#'   Java errors are used, for instance, by then MIT licensed Picard tools,
#'   \url{http://broadinstitute.github.io/picard/}
#'
#' @seealso \code{\link{Exception}}
#'
#' @export
#' @name SamFileFormatException
NULL

SamFileFormatMsg <- function (path= NA, line= NA, data= NA, wd= getwd() ) {
	sprintf( 'A SamFileFormatException occurred at line %i in sam file: "%s".'
	 			%pp% '(Running in: "%s").\n'
	 			%p% 'That line began: "%s".',
	 	line, path, wd, substr(data, 1, 80))
}
SamFileHeaderMsg <- function( path= NA, line= NA, data= NA, wd= getwd() ) {
	sprintf( 'A SamFileHeaderException occurred parsing the sam file header'
	 		   %pp% 'at line %i in file: "%s". (Running in: "%s").\n'
	 			%p% 'That line began: "%s".',
		line, path, wd, substr(data, 1, 80))
}
SamFileReadMsg <- function( path= NA, line= NA, data= NA, wd= getwd() ) {
	sprintf( 'A SamFileReadException occurred parsing the sam file read'
				%pp% 'at line %i in file: "%s". (Running in: "%s").\n'
				%p% 'That line began: "%s".',
		line, path, wd, substr(data, 1, 80))
}
MISSING_HEADER_Msg <- function( path= NA, wd= getwd() ) {
	sprintf( 'A MISSING_HEADER Exception occurred in sam file: "%s".'
	 		   %pp% '(Running in: "%s").\n'
	 			%p% 'Sam files require headers.'
				%pp% 'Temporary files with just reads are not true sam files.',
	path, wd)
}
HeaderOnlyMsg <- function( path= NA, wd= getwd() ) {
	sprintf( 'A HeaderOnlyException occurred in sam file: "%s".'
				%pp% '(Running in: "%s").\n'
				%p% 'Sam files require reads.'
				%pp% 'Temporary files with just headers are not true sam files.',
				path, wd)
}

#' @describeIn SamFileFormatException
#'
#' This exception and sub-classes describe problems with sam files.
#' \itemize{
#'    \item message = [\var{package}] A SamFileFormatException occurred at line
#'    \var{line} in sam file: "\var{file}". (Running in: "\var{getwd()}"). That
#'    line began: "\var{first 80 char of data}".
#'    \item class = \code{c( "SamFileFormatException", "FileFormatException",
#'    "DataException", "Exception", "condition" )}
#' }
#'
#' @usage \code{ SamFileFormatException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
SamFileFormatException <- function( path=NA, line=NA, data=NA,
	message= SamFileFormatMsg( path=path, line=line, data=data ),
	call= NULL, package= packageName(), ...
) {
	ffEx <- FileFormatException( message=message, call= call, package= package,
		data=data, path=path, line=line, ... )
	extendException( "SamFileFormatException", ffEx )
}

#' @describeIn SamFileFormatException
#'
#' This exception and sub-classes describe problems with sam file headers.
#' \itemize{
#'    \item message = [\var{package}] A SamFileHeaderException occurred parsing
#'    the sam file header at line \var{line} in file: "\var{file}". (Running in:
#'    "\var{getwd()}"). That line began: "\var{first 80 char of data}".
#'    \item class = \code{c( "SamFileHeaderException", "SamFileFormatException",
#'    "FileFormatException", "DataException", "Exception", "condition" )}
#' }
#'
#' @usage \code{ SamFileHeaderException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
SamFileHeaderException <- function( path=NA, line=NA, data=NA,
	message= SamFileHeaderMsg(line=line, path=path, data=data),
	call= NULL, package= packageName(), ...
) {
	sfhEx <- SamFileFormatException( message=message, call= call, package= package,
										 data=data, path=path, line=line, ... )
	extendException( "SamFileHeaderException", sfhEx )
}


#' @describeIn SamFileFormatException
#'
#' This exception and sub-classes describe problems with sam file reads.
#' \itemize{
#'    \item message = [\var{package}] A SamFileReadException occurred parsing
#'    the sam file read at line \var{line} in file: "\var{file}". (Running in:
#'    "\var{getwd()}"). That line began: "\var{first 80 char of data}".
#'    \item class = \code{c( "SamFileReadException","SamFileFormatException",
#'    "FileFormatException", "DataException", "Exception", "condition" )}
#' }
#'
#' @usage \code{ SamFileReadException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
SamFileReadException <- function( path=NA, line=NA, data=NA,
	message= SamFileReadMsg( line=line, path=path, data=data),
	call= NULL, package= packageName(), ...
) {
	sfrEx <- SamFileFormatException( message=message, call= call, package= package,
												data=data, path=path, line=line, ... )
	extendException( "SamFileReadException", sfrEx )
}

#' @describeIn SamFileFormatException
#'
#' Sam files require headers. Temporary files with just reads are not true sam
#' files.
#' \itemize{
#'    \item message = [\var{package}] A MISSING_HEADER Exception occurred in sam
#'    file: "\var{file}". (Running in: "\var{getwd()}"). Sam files require
#'    headers. Temporary files with just reads are not true sam files.'
#'    \item class = \code{c( "MISSING_HEADER_Exception",
#'    "SamFileHeaderException", "SamFileFormatException", "FileFormatException",
#'    "DataException", "Exception", "condition" )}
#' }
#' @usage \code{ MISSING_HEADER_Exception( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
MISSING_HEADER_Exception <- function( path=NA, data=NA, line=NA,
	message= MISSING_HEADER_Msg( path= path ), call= NULL, package= packageName(), ...
) {
	sfhEx <- SamFileHeaderException( message=message, call= call, package= package,
												path=path, data=data, line=line, ... )
	extendException( "MISSING_HEADER_Exception", sfhEx )
}


#' @describeIn SamFileFormatException
#'
#' Sam files require reads. Temporary files with just headers are not true sam
#' files.
#' \itemize{
#'    \item message = [\var{package}] A HeaderOnlyException occurred in sam
#'    file: "\var{file}". (Running in: "\var{getwd()}"). Sam files require
#'    reads. Temporary files with just headers are not true sam files.'
#'    \item class = \code{c( "HeaderOnlyException",
#'    "SamFileReadException", "SamFileFormatException", "FileFormatException",
#'    "DataException", "Exception", "condition" )}
#' }
#' @usage \code{ HeaderOnlyException( path = NA, line = NA, data = NA,
#'    message = *, call = NULL, package = packageName(), ...)}
#'
#' @export
HeaderOnlyException <- function( path=NA, data=NA, line= NA,
	message= HeaderOnlyMsg( path= path ), call= NULL, package= packageName(), ...
) {
	sfrEx <- SamFileReadException( message=message, call= call, package= package,
				path=path, data=data, line=line, ... )
	extendException( "HeaderOnlyException", sfrEx )
}

samErrors <- list(
   'MISSING_SEQUENCE_DICTIONARY' = list(
      groups= c('samHeaderError'),
      message= "There is no sequence dictionary in the header"
   ),
   'MISSING_READ_GROUP' = list(
      groups= c('samHeaderError'),
      message= "The header is missing read group information"
   ),
   'UNRECOGNIZED_HEADER_TYPE' = list(
      groups= c('samHeaderError'),
      message= "Header record is not one of the standard types"
   ),
   'POORLY_FORMATTED_HEADER_TAG' = list(
      groups= c('samHeaderError'),
      message= "Header tag does not have colon"
   ),
   'HEADER_TAG_MULTIPLY_DEFINED' = list(
      groups= c('samHeaderError'),
      message= "Header tag appears more than once in header line with different value"
   ),
   'HEADER_RECORD_MISSING_REQUIRED_TAG' = list(
      groups= c('samHeaderError'),
      message= ""
   ),
   'DUPLICATE_READ_GROUP_ID' = "Same read group id appears more than once",
   'MISSING_PLATFORM_VALUE' = "The read group is missing its PL (platform unit) field",
   'INVALID_PLATFORM_VALUE' = "The read group has an invalid value set for its PL field",
   'DUPLICATE_PROGRAM_GROUP_ID' = "Same program group id appears more than once",
   'MISSING_VERSION_NUMBER' = "",
   'INVALID_VERSION_NUMBER' = "",

   'INVALID_FLAG_PROPER_PAIR' = "Proper pair flag set for unpaired read.",
   'INVALID_FLAG_MATE_UNMAPPED' = "Mate unmapped flag set when mate is mapped or not set when mate is not mapped",
   'MISMATCH_FLAG_MATE_UNMAPPED' = "Mate unmapped flag does not match read unmapped flag of mate",
   'INVALID_FLAG_MATE_NEG_STRAND' = "Mate negative strand flag set for unpaired read",
   'MISMATCH_FLAG_MATE_NEG_STRAND' = "Mate negative strand flag does not match read negative strand flag of mate",
   'INVALID_FLAG_FIRST_OF_PAIR' = "First of pair flag set for unpaired read",
   'INVALID_FLAG_SECOND_OF_PAIR' = "Second of pair flag set for unpaired read",
   'PAIRED_READ_NOT_MARKED_AS_FIRST_OR_SECOND' = "Pair flag set but not marked as first or second of pair",
   'INVALID_FLAG_NOT_PRIM_ALIGNMENT' = "Not primary alignment flag set for unmapped read",
   'INVALID_FLAG_SUPPLEMENTARY_ALIGNMENT' = "Supplementary alignment flag set for unmapped read",
   'INVALID_FLAG_READ_UNMAPPED' =  "Mapped read flat not set for mapped read",

   'INVALID_QUALITY_FORMAT' = "Quality encodings out of range; appear to be Solexa or Illumina when Phread expected.",
   'INVALID_INSERT_SIZE' = "Inferred insert size is out of range",
   'INVALID_MAPPING_QUALITY' = "Mapping quality set for unmapped read or is >= 256",
   'INVALID_CIGAR' = "CIGAR string is empty for mapped read or not empty of unmapped read, or other CIGAR badness",
   'ADJACENT_INDEL_IN_CIGAR' = "CIGAR string contains I followed by D, or vice versa",
   'INVALID_MATE_REF_INDEX' = "Mate reference index (MRNM) set for unpaired read",
   'MISMATCH_MATE_REF_INDEX' = "Mate reference index (MRNM) does not match reference index of mate",
   'INVALID_REFERENCE_INDEX' = "Reference index not found in sequence dictionary",
   'INVALID_ALIGNMENT_START' = "Alignment start is can not be correct",
   'MISMATCH_MATE_ALIGNMENT_START' = "Mate alignment does not match alignment start of mate",
   'MATE_FIELD_MISMATCH' = "The record's mate fields do not match the corresponding fields of the mate",
   'INVALID_TAG_NM' = "The NM tag (nucleotide differences) is incorrect",
   'MISSING_TAG_NM' = "The NM tag (nucleotide differences) is missing",
   'RECORD_OUT_OF_ORDER' = "The record is out of order",
   'READ_GROUP_NOT_FOUND' = "A read group ID on a SAMRecord is not found in the header",
   'RECORD_MISSING_READ_GROUP' = "A SAMRecord is found with no read group id",
   'INVALID_INDEXING_BIN' = "Indexing bin set on SAMRecord does not agree with computed value",
   'TRUNCATED_FILE' = "",
   'MISMATCH_READ_LENGTH_AND_QUALS_LENGTH' = "",
   'EMPTY_READ' = "",
   'CIGAR_MAPS_OFF_REFERENCE' = "Bases corresponding to M operator in CIGAR are beyond the end of the reference",
   'MISMATCH_READ_LENGTH_AND_E2_LENGTH' = "Length of E2 (secondary base calls) tag values should match read length",
   'MISMATCH_READ_LENGTH_AND_U2_LENGTH' = "Length of U2 (secondary base quals) tag values should match read length",
   'E2_BASE_EQUALS_PRIMARY_BASE' = "Secondary base calls should not be the same as primary, unless one or the other is N",
   'BAM_FILE_MISSING_TERMINATOR_BLOCK' = "BAM appears to be healthy, but is an older file so doesn't have terminator block",
   'INVALID_DATE_STRING' = "Date string is not ISO-8601",
   'TAG_VALUE_TOO_LARGE' = "Unsigned integer tag value is deprecated in BAM",
   'INVALID_INDEX_FILE_POINTER' = "Invalid virtualFilePointer in index",
   'INVALID_PREDICTED_MEDIAN_INSERT_SIZE' = "PI tag value is not numeric",

   'MATE_NOT_FOUND' = "Read is marked as paired, but its pair was not found",
   'MATES_ARE_SAME_END' = "Both mates are marked as first of pair, or both mates are marked as second of pair",
   'MISMATCH_MATE_CIGAR_STRING' = "The Cigar String in the MC Tag does not match the Cigar String for the mate of this read",
   'MATE_CIGAR_STRING_INVALID_PRESENCE' = "There is a Cigar String (stored in the MC Tag) for a read whose mate is NOT mapped"
)


