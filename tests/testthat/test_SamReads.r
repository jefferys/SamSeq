context("Testing the SamReads object functionality")

samFile <- file.path( "goodSamFiles", "pe.sam" )

describe( "Test fixtures for use in the test_sam testthat file", {
	it( "Provides 'samFile' as a generic input sam file.", {
		expect_true( file.exists( samFile ))
	})
})

describe( "Construct the appropriate reads object with parsed tags", {

	expect_output( sam <- Sam(samFile) )
	reads <- SamReads(sam)
	r <- as.data.frame(SamReads(sam))

	it( "Can consturct a reads object from a Sam object", {
		expect_s3_class(reads, "SamReads")
	})

	it( "Has the same source object as the sam file it came from", {
		expect_equal( SamSource(reads), SamSource(sam))
	})

	it( "Acts as a data frame with the correct columns and rows", {
		expect_equal(nrow(r), 4)
		expect_equal(ncol(r), 16)
		wantNames <- c( "qname", "flag", "rname", "pos", "mapq", "cigar", "rnext",
							"pnext", "tlen", "seq", "qual", "NH", "HI", "AS", "nM", "xT" )
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
		expect_equal( class(r[["xT"]]), "character")
	})
	it( "Missing values in optional tags become NA's", {
		expect_equal( r$xT, as.character(c(NA, "Test Me", NA, NA)))
	})
})
describe( "Construct the appropriate reads object without parsed tags", {

	expect_output( sam <- Sam(samFile, splitTags= FALSE) )
	reads <- SamReads(sam)
	r <- as.data.frame(SamReads(sam))

	it( "Can consturct a reads object from a Sam object", {
		expect_s3_class(reads, "SamReads")
	})

	it( "Has the same source object as the sam file it came from", {
		expect_equal( SamSource(reads), SamSource(sam))
	})

	it( "Acts as a data frame with the correct columns and rows", {
		expect_equal(nrow(r), 4)
		expect_equal(ncol(r), 12)
		wantNames <- c( "qname", "flag", "rname", "pos", "mapq", "cigar", "rnext",
							 "pnext", "tlen", "seq", "qual", "tags" )
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
		expect_equal( class(r[["tags"]]), "character")
	})
	it( "Tags are unparsed", {
		expect_equal( length( unlist( strsplit( r$tags[1], split= "\t" ))), 4 )
		expect_equal( length( unlist( strsplit( r$tags[2], split= "\t" ))), 5 )
	})
})

describe( "countAllReads", {
	it( "Returns the number of reads expected", {
		expect_output( sam <- Sam(samFile) )
		expect_equal( countAllReads( sam ), 4 )
		expect_equal( countAllReads( SamReads( sam )), 4 )
	})
})
