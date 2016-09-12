context("Testing the SamSource object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

describe( "Test fixtures for use in the test_sam testthat file", {
	it( "Provides 'samFile' as a generic input sam file.", {
		expect_true( file.exists( samFile ))
	})
})

describe( "Can construct a source object", {

	expect_output( sam <- Sam(samFile) )

	it( "Can consturct a source object from a Sam object", {
		source <- SamSource(sam)
		expect_s3_class(source, "SamSource")
	})
})

