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
describe( "Constructing header objects with Sam()", {
	it( "Works on a Sam object", {
		expect_output( sam <- Sam(samFile) )
		header <- SamHeader(sam)
		expect_s3_class(header, "SamHeader")
		expect_s3_class(SamSource(header), "SamSource" )
	})
	it( "Works on a character vector of lines", {
		header <- SamHeader(lines)
		expect_s3_class(header, "SamHeader")
		expect_s3_class(SamSource(header), "SamSource" )
	})
})
describe( "Constructing a header data frame", {
	it( "Can cast a SamHeader derived from a file Sam object", {
		expect_output( sam <- Sam(samFile) )
		header <- SamHeader(sam)
		headerDF <- as.data.frame(header)
		expect_equal( names(headerDF), c("tag", "record"))
		expect_equal( nrow(headerDF), 6)
	})
	it( "Can cast a SamHeader derived from a character vector Sam object", {
		header <- SamHeader(lines)
		headerDF <- as.data.frame(header)
		expect_equal( names(headerDF), c("tag", "record"))
		expect_equal(nrow(headerDF), length(lines))
		expect_equal(headerDF$tag, tags)
		expect_equal(headerDF$record, records)
	})
	it( "Built directly from a character vector", {
		headerDF <- parseSamHeaderLines(lines)
		expect_s3_class( headerDF, "data.frame" )
		expect_equal( names(headerDF), c("tag", "record"))
		expect_equal(nrow(headerDF), length(lines))
		expect_equal(headerDF$tag, tags)
		expect_equal(headerDF$record, records)
	})
})
