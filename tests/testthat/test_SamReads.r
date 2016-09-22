context("Testing the SamReads object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

describe( "Test fixtures for use in the test_sam testthat file", {
	it( "Provides 'samFile' as a generic input sam file.", {
		expect_true( file.exists( samFile ))
	})
})

describe( "Construct the appropriate reads object", {

	expect_output( sam <- Sam(samFile) )
	r <- SamReads(sam)

	it( "Can consturct a reads object from a Sam object", {
		expect_s3_class(r, "SamReads")
		expect_s3_class(r, "data.frame")
	})

	it( "Has the same source object as the sam file it came from", {
		expect_equal( SamSource(r), SamSource(sam))
	})

	it( "Acts as a data frame with the correct columns and rows", {
		expect_equal(nrow(r), 4)
		expect_equal(ncol(r), 15)
		wantNames <- c( "qname", "flag", "rname", "pos", "mapq", "cigar", "rnext",
							"pnext", "tlen", "seq", "qual", "NH", "HI", "AS", "nM" )
		expect_equal(names(r), wantNames)
		expect_equal( class(r[["qname"]]), "character")
		expect_equal( class(r[["flag"]]), "integer")
		expect_equal( class(r[["rname"]]), "character")
		expect_equal( class(r[["pos"]]), "integer")
		expect_equal( class(r[["mapq"]]), "integer")
		expect_equal( class(r[["cigar"]]), "character")
		expect_equal( class(r[["rnext"]]), "character")
		expect_equal( class(r[["pnext"]]), "integer")
		expect_equal( class(r[["tlen"]]), "integer")
		expect_equal( class(r[["seq"]]), "character")
		expect_equal( class(r[["qual"]]), "character")
		expect_equal( class(r[["NH"]]), "integer")
		expect_equal( class(r[["HI"]]), "integer")
		expect_equal( class(r[["AS"]]), "integer")
		expect_equal( class(r[["nM"]]), "integer")
	})
})
