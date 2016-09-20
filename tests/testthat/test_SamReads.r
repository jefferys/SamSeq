context("Testing the SamReads object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

describe( "Test fixtures for use in the test_sam testthat file", {
	it( "Provides 'samFile' as a generic input sam file.", {
		expect_true( file.exists( samFile ))
	})
})

describe( "Can construct a reads object", {

	expect_output( sam <- Sam(samFile) )

	it( "Can consturct a reads object from a Sam object", {
		reads <- SamReads(sam)
		expect_s3_class(reads, "SamReads")
		expect_s3_class(reads, "data.frame")
	})
})
