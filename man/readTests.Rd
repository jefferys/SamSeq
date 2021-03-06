% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samFilters.R
\name{readTests}
\alias{readTests}
\alias{areReadFlagsSet}
\alias{areReadFlagsUnset}
\alias{areReadFlags}
\alias{readHasRef}
\alias{readMateHasRef}
\title{Read tests}
\usage{
areReadFlagsSet(read, flags)

areReadFlagsUnset(read, flags)

areReadFlags(read, flagVec)

readHasRef(read, ref)

readMateHasRef(read, ref)
}
\arguments{
\item{read}{The read to test.}

\item{flags}{A character vector of one or more flag names to test against
the flags set or unset for a read.}

\item{flagVec}{A logical vector where the names are sam flags and the values
indicate if that flag shoud be set (\code{TRUE}) or unset (\code{FLAG}). If
the value of a flag does not matter, it should not be included. NA should not
be used.}

\item{ref}{Regular expression matched against the reference aligned to.
Keeps a read if \code{ref} matches to the reference this read aligns to
(\code{readHasRef()}, matching against \code{rname}) or the reference the
mate of this read aligns to (\code{readMateHasRef()}, matching against
\code{rnext}).}
}
\value{
Returns \code{TRUE} if a read passes the test, \code{FALSE} otherwise.
Different ends of the same template will return the same result.
}
\description{
Tests a read to see if its meets some criteria. May return \code{TRUE} for
only one end of a paired end read. These tests are suitable for use in a
\code{\link{samReadFilter}} but can result in unpaired reads after
application; \code{pairedReads} should probably be run after running any
of these as filters.
}
\section{Functions}{
\itemize{
\item \code{areReadFlagsSet}: Returns \code{TRUE} if all the specified flags are set for
the read. Equivalent to \code{\link{allSetSamFlags}(read$flag, flags)}

\item \code{areReadFlagsUnset}: Returns \code{TRUE} if all the specified flags are set for
the read. Equivalent to \code{\link{allUnsetSamFlags}(read$flag, flags)}

\item \code{areReadFlags}: Returns \code{TRUE} if all the flags that set in the
flagVec are set for the read and all the flags that are unset in the
flagVec are unset in the read. If all flags in the flagVec checked are set
or all are unset, it is more efficient to use \code{areReadFlagsSet} or
\code{areReadFlagsUnset}.

\item \code{readHasRef}: Returns \code{TRUE} if the read is aligned to a
reference identified by the specified regular expressions (matched against
the reads \code{rname}).

\item \code{readMateHasRef}: Returns \code{TRUE} if the read's mate (other paired
end) is aligned to a  reference identified by the specified regular
expressions (matched against the reads \code{rnext}).
}}

\examples{
\dontrun{
sam <- Sam("someFile")
aRead <- SamReads(sam)[1]

areReadFlagsUnset( aRead, "NOT_PRIMARY_ALIGNMENT" )
primaryReads <- samReadFilter( sam, "areReadFlagsUnset", "NOT_PRIMARY_ALIGNMENT" )

areReadFlagsSet( aRead, "NOT_PRIMARY_ALIGNMENT" )
secondaryReads <- samReadFilter( sam, "areReadFlagsSet", "NOT_PRIMARY_ALIGNMENT" )

goodAlignmentFlagVec <- c(
	"READ_PAIRED"           =  TRUE, "PROPER_PAIR"             =  TRUE,
   "READ_UNMAPPED"         = FALSE, "MATE_UNMAPPED"           = FALSE,
   "NOT_PRIMARY_ALIGNMENT" = FALSE, "READ_FAILS_VENDOR_QC"    = FALSE,
   "DUPLICATE_READ"        = FALSE, "SUPPLEMENTARY_ALIGNMENT" = FALSE
)
areReadFlags( aRead, goodAlignmentFlagVec )
goodReads <- samReadFilter( sam, "areReadFlags", goodAlignmentFlagVec )
}
}
