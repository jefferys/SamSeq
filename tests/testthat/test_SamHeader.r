context("Testing the SamHeader object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

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
	})

})
