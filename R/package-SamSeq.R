#' Work with aligned reads from a SAM file
#'
#' Work with the aligned read data as provided in a *.sam file, as in the format
#' used by SamTools. Tracks header information, reads, and the source file.
#' Loads all reads and optionally the extra read tags. Includes functions to
#' translate the Sam flags and to filter reads.
#'
#' @name SamSeq
#' @docType package
#'
#' @import Exception
#' @importFrom JefferysRUtils %p% %pp% expect_equalOrNull
#' @importFrom utils packageName
NULL
