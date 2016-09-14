context("Testing the SamSource object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

describe( "SamSource() constructs create correct objects", {
	samSourceData <- list(name="aSourceName", type="file", host="bioinfo.unc.edu")
	it( "Works for list input", {
		obj <- SamSource( samSourceData )
		expect_equal(samSourceName(obj), samSourceData$name )
		expect_equal(samSourceHost(obj), samSourceData$host )
		expect_equal(samSourceType(obj), samSourceData$type )
	})
	it( "Works for character input", {
		obj <- SamSource( samSourceData$name,
								host= samSourceData$host, type= samSourceData$type )
		expect_equal(samSourceName(obj), samSourceData$name )
		expect_equal(samSourceHost(obj), samSourceData$host )
		expect_equal(samSourceType(obj), samSourceData$type )
	})
})

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

