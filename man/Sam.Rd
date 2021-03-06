% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sam.R
\name{Sam}
\alias{Sam}
\title{Create a sam (sequence alignment/map) object.}
\usage{
Sam(file, splitTags = TRUE)
}
\arguments{
\item{file}{A *.sam file name.}

\item{splitTags}{Parse optional sam file read tags into columns. True by
default, but increases processing time a factor of 4 or so. If false will
have only one "tags" column with a single tab-delimited string.}
}
\value{
An object of class Sam.
}
\description{
An object of class \code{Sam} represents a \file{*.sam} file. This is
supposed to be an opaque object for use by other functions. The size of a
\code{sam} object is limited to only what can be read into memory and
manipulated, which is probably at best about 1/3 of the total memory on the
system (due to \R functions being "copy by value".) No support is provided
for \file{*.bam} files.
}
\details{
A sam object is composed of three parts, a \code{\link{SamHeader}},
\code{\link{SamReads}}, and \code{\link{SamSource}} data. These parts can
be extracted as objects of the same name by constructor functions of the same
name applied to the \code{Sam} object. Any function that works when applied
to a component object will work on the \code{Sam} object too.
}
\examples{
\dontrun{
samObj <- Sam( 'myFile.sam' )

myHeader <- samHeader(samObj)
myReads <- samReads(samObj)
mySource <- samSource(samObj)
}
}
