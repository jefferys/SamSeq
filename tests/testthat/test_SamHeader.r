context("Testing the SamHeader object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )
tags <- c("HD", "SQ", "SQ", "SQ", "PG", "CO")
records <- c( "VN:1.4",
		"SN:chr7	LN:159138663",
		"SN:chr22	LN:51304566",
		"SN:gi|333031|lcl|HPV16REF.1|	LN:7906",
		"ID:STAR	PN:STAR	VN:STAR_2.4.2a	CL:STAR   --genomeDir Genomes   --readFilesIn unaligned_1.fastq   unaligned_2.fastq      --limitOutSAMoneReadBytes 1000000   --outFileNamePrefix STAR_virus_   --outFilterMultimapNmax 1080   --outFilterMismatchNmax 10   --chimSegmentMin 10",
		"user command line: STAR --genomeDir Genomes --readFilesIn unaligned_1.fastq unaligned_2.fastq --runThreadN 1 --outFilterMismatchNmax 10 --outFilterMultimapNmax 1080 --chimSegmentMin 10 --limitOutSAMoneReadBytes 1000000 --outFileNamePrefix STAR_virus_" )
lines <- paste( paste0("@", tags), records, sep="\t" )

describe( "Test fixtures for use in the test_sam testthat file", {
	it( "Provides 'samFile' as a generic input sam file.", {
		expect_true( file.exists( samFile ))
	})
})

describe( "Can construct a header object", {

	expect_output( sam <- Sam(samFile) )
	it( "Can consturct a header object from a Sam object", {
		header <- SamHeader(sam)
		expect_s3_class(header, "SamHeader")
		expect_s3_class(header, "data.frame")
		expect_equal( names(header), c("tag", "record"))
	})
	it( "Can consturcts a header object from a character vector", {
		header <- SamHeader(lines)
		expect_s3_class(header, "SamHeader")
		expect_s3_class(header, "data.frame")
		expect_equal( names(header), c("tag", "record"))

		expect_equal(nrow(header), length(lines))
		expect_equal(header$tag, tags)
		expect_equal(header$record, records)
	})
})

describe( "Can parse character vectors of header lines", {
	headerDF <- parseSamHeaderLines(lines)
	expect_equal(nrow(headerDF), length(lines))
	expect_equal(headerDF$tag, tags)
	expect_equal(headerDF$record, records)
})
