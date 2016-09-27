context("Testing sam filters")

samFile <- file.path( "goodSamFiles", "pe.sam" )
expect_output( sam <- Sam(samFile) )
reads <- as.data.frame(SamReads(sam))

describe( "samReadFilter", {
	it( "Returns a sam object", {
		readName <- reads[1,"qname"]
		got <- samReadFilter( sam, function(x) {x$qname == readName} )
		expect_s3_class( got, "Sam" )
		expect_s3_class( SamReads(got), "SamReads" )
		expect_s3_class( SamHeader(got), "SamHeader" )
		expect_s3_class( SamSource(got), "SamSource" )
	})
	it( "Reads are returned only where function returns TRUE", {
		readName <- reads[1,"qname"]
		got <- as.data.frame(SamReads(samReadFilter( sam, function(x) {x$qname == readName} )))
		expect_true(nrow(got) < nrow(reads), "Filtered something")
		want <- reads[reads[, "qname"] == readName, ]
		expect_equivalent( got, want, "Read DF should be the same, but not attr." )
	})
})

describe( "Tests that may not preserve read pairs", {
	describe( "areReadFlagsSet", {
		it( "Returns true when all flags set", {
			expect_true(areReadFlagsSet(reads[1,], "FIRST_OF_PAIR"))
			expect_true(areReadFlagsSet(reads[2,], c("READ_PAIRED", "SECOND_OF_PAIR")))
			expect_true(areReadFlagsSet(reads[3,], c("FIRST_OF_PAIR", "READ_PAIRED")))
			expect_true(areReadFlagsSet(reads[4,], c("SECOND_OF_PAIR")))
		})
		it( "Returns false when any flags unset", {
			expect_false(areReadFlagsSet(reads[1,], "SECOND_OF_PAIR"))
			expect_false(areReadFlagsSet(reads[2,], c("READ_PAIRED", "FIRST_OF_PAIR")))
			expect_false(areReadFlagsSet(reads[3,], c("SECOND_OF_PAIR", "READ_PAIRED")))
			expect_false(areReadFlagsSet(reads[4,], c("FIRST_OF_PAIR")))

		})
		it( "Works as a filter", {
			got <- as.data.frame( SamReads(
				samReadFilter( sam, "areReadFlagsSet", "FIRST_OF_PAIR" )))
			want <- reads[c(1,3),]
			expect_equal(got, want)
		})
	})
	describe( "areReadFlagsUnset", {
		it( "Returns true when all flags are unset", {
			expect_true(areReadFlagsUnset(reads[1,], "SECOND_OF_PAIR"))
			expect_true(areReadFlagsUnset(reads[2,], c("READ_UNMAPPED", "FIRST_OF_PAIR")))
			expect_true(areReadFlagsUnset(reads[3,], c("SECOND_OF_PAIR", "READ_UNMAPPED")))
			expect_true(areReadFlagsUnset(reads[4,], c("FIRST_OF_PAIR")))
		})
		it( "Returns false when any flags set", {
			expect_false(areReadFlagsUnset(reads[1,], "FIRST_OF_PAIR"))
			expect_false(areReadFlagsUnset(reads[2,], c("READ_UNMAPPED", "SECOND_OF_PAIR")))
			expect_false(areReadFlagsUnset(reads[3,], c("FIRST_OF_PAIR", "READ_UNMAPPED")))
			expect_false(areReadFlagsUnset(reads[4,], c("SECOND_OF_PAIR")))
		})
		it( "Works as a filter", {
			got <- as.data.frame( SamReads( samReadFilter(
				sam, "areReadFlagsUnset", "FIRST_OF_PAIR" )))
			want <- reads[c(2,4),]
			expect_equal(got, want)
		})

	})
	describe( "areReadFlags", {
		it( "Works for all true flags", {
			expect_true(areReadFlags(reads[1,], c("FIRST_OF_PAIR"= TRUE)))
			expect_true(areReadFlags(reads[1,],
										c("READ_PAIRED"=TRUE, "FIRST_OF_PAIR"= TRUE)))
			expect_false(areReadFlags(reads[1,],
										c("READ_PAIRED"=TRUE, "SECOND_OF_PAIR"= TRUE)))
			expect_false(areReadFlags(reads[1,],
										c("SECOND_OF_PAIR"=TRUE, "READ_PAIRED"= TRUE)))
		})
		it( "Works for all false flags", {
			expect_true(areReadFlags(reads[1,], c("SECOND_OF_PAIR"= FALSE)))
			expect_true(areReadFlags(reads[1,],
											 c("READ_UNMAPPED"= FALSE, "SECOND_OF_PAIR"= FALSE)))
			expect_false(areReadFlags(reads[1,], c("FIRST_OF_PAIR"= FALSE)))
			expect_false(areReadFlags(reads[1,],
										c("FIRST_OF_PAIR"= FALSE, "READ_UNMAPPED"= FALSE)))
			expect_false(areReadFlags(reads[1,],
										c("READ_UNMAPPED"= FALSE, "FIRST_OF_PAIR"= FALSE)))
		})
		it( "Works for one true and one false flag", {
			expect_true(areReadFlags(reads[1,],
									c( "FIRST_OF_PAIR"= TRUE, "SECOND_OF_PAIR"= FALSE )))
			expect_true(areReadFlags(reads[1,],
									c( "SECOND_OF_PAIR"= FALSE, "FIRST_OF_PAIR"= TRUE )))
			expect_false(areReadFlags(reads[1,],
									c( "FIRST_OF_PAIR"= FALSE, "SECOND_OF_PAIR"= TRUE )))
			expect_false(areReadFlags(reads[1,],
									c( "SECOND_OF_PAIR"= TRUE, "FIRST_OF_PAIR"= FALSE )))
			expect_false(areReadFlags(reads[1,],
											  c( "FIRST_OF_PAIR"= FALSE, "READ_PAIRED"= TRUE )))
			expect_false(areReadFlags(reads[1,],
											  c( "READ_PAIRED"= TRUE, "FIRST_OF_PAIR"= FALSE )))
			expect_false(areReadFlags(reads[1,],
											  c( "SECOND_OF_PAIR"= FALSE, "READ_UNMAPPED"= TRUE )))
			expect_false(areReadFlags(reads[1,],
											  c( "READ_UNMAPPED"= TRUE, "SECOND_OF_PAIR"= FALSE )))
		})
		it( "Works as a filter", {
			got <- as.data.frame( SamReads( samReadFilter( sam, "areReadFlags",
									c( "FIRST_OF_PAIR"= TRUE, "SECOND_OF_PAIR"= FALSE ))))
			want <- reads[c(1,3),]
			expect_equal(got, want)
		})
	})
	describe( "readHasRef", {
		it( "Returns TRUE if rname matches given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_true( readHasRef( read, "chr1" ))
			expect_true( readHasRef( read, "chr1$" ))
			expect_true( readHasRef( read, "^chr1$" ))
		})
		it( "Returns FALSE if rname does not match given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_false( readHasRef( read, "^Bob$" ))
			expect_false( readHasRef( read, "chr12" ))
		})
	})
	describe( "readMateHasRef", {
		it( "Returns TRUE if rnext matches given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_true( readMateHasRef( read, "chr12" ))
			expect_true( readMateHasRef( read, "chr12$" ))
			expect_true( readMateHasRef( read, "^chr12$" ))
		})
		it( "Returns FALSE if rnext does not match given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_false( readMateHasRef( read, "Bob" ))
			expect_false( readMateHasRef( read, "^chr1$" ))
		})
	})
})

describe( "Tests preserving read pairs", {
	describe( "pairIsAligned", {
		read <- reads[1,]
		it( "Returns TRUE if both READ_UNMAPPED and MATE_UNMAPPED are unset", {
			read$flag <- samFlags( "READ_PAIRED" )
			expect_true( pairIsAligned(read) )
		})
		it( "Returns FALSE if either READ_UNMAPPED or MATE_UNMAPPED are set", {
			read$flag <- samFlags( "READ_UNMAPPED" )
			expect_false( pairIsAligned(read) )
			read$flag <- samFlags( "MATE_UNMAPPED" )
			expect_false( pairIsAligned(read) )
			read$flag <- samFlags( c("READ_UNMAPPED", "MATE_UNMAPPED"))
			expect_false( pairIsAligned(read) )
		})
	})
	describe( "pairIsPrimary", {
		read <- reads[1,]
		it( "TRUE if NOT_PRIMARY_ALIGNMENT, READ_UNMAPPED, and MATE_UNMAPPED unset", {
			read$flag <- samFlags( "READ_PAIRED" )
			expect_true( pairIsPrimary(read) )
		})
		it( "FALSE if NOT_PRIMARY_ALIGNMENT, READ_UNMAPPED, or MATE_UNMAPPED set", {
			read$flag <- samFlags( "READ_UNMAPPED" )
			expect_false( pairIsPrimary(read) )
			read$flag <- samFlags( "MATE_UNMAPPED" )
			expect_false( pairIsPrimary(read) )
			read$flag <- samFlags( "NOT_PRIMARY_ALIGNMENT" )
			expect_false( pairIsPrimary(read) )
			read$flag <- samFlags( c("READ_UNMAPPED", "MATE_UNMAPPED"))
			expect_false( pairIsPrimary(read) )
			read$flag <- samFlags( c("READ_UNMAPPED", "NOT_PRIMARY_ALIGNMENT"))
			expect_false( pairIsPrimary(read) )
			read$flag <- samFlags( c("MATE_UNMAPPED", "NOT_PRIMARY_ALIGNMENT"))
			expect_false( pairIsPrimary(read) )
		})
	})
	describe( "pairHasRef", {
		it( "Returns TRUE if either rname or rnext or both match given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_true( pairHasRef( read, "chr1" ))
			expect_true( pairHasRef( read, "chr1$" ))
			expect_true( pairHasRef( read, "chr12$" ))
		})
		it( "Returns FALSE if neither rname nor rnext match given RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_false( pairHasRef( read, "chr.1$" ))
		})
	})
	describe( "pairIsChimeric", {
		it( "Returns TRUE if rname and rnext match different REs", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_true( pairIsChimeric( read, "chr1$", "chr12$" ))
			expect_true( pairIsChimeric( read, "chr12$", "chr1$" ))
		})
		it( "Returns FALSE if both only match same name", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_false( pairIsChimeric( read, "chr1$", "chr.$" ))
			expect_false( pairIsChimeric( read, "chr12", "chr.2" ))
		})
		it( "Returns FALSE if one or both matches no name", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr12"
			expect_false( pairIsChimeric( read, "chr1$", "chr3" ))
			expect_false( pairIsChimeric( read, "chr3", "chr1$" ))
			expect_false( pairIsChimeric( read, "chr12", "bob" ))
			expect_false( pairIsChimeric( read, "bob", "chr12" ))
			expect_false( pairIsChimeric( read, "me", "you" ))
		})
	})
	describe( "pairIsNotChimeric", {
		it( "Returns TRUE if both rname and rnext match same RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr1"
			expect_true( pairIsNotChimeric( read, "^chr1$" ))
		})
		it( "Returns FALSE if either rname or rnext or both fail to match same RE", {
			read <- reads[1,]
			read$rname <- "chr1"; read$rnext <- "chr2"
			expect_false( pairIsNotChimeric( read, "^chr1$" ))
			expect_false( pairIsNotChimeric( read, "^chr2$" ))
			expect_false( pairIsNotChimeric( read, "^chr3$" ))
		})
	})
})

describe( "pairedReads", {
	describe( "Filters unpaired reads based on SECOND_OF_PAIR or FIRST_OF_PAIR", {
		expect_equal(pairedReads(sam), sam)

		sam2 <- sam
		sam2$reads[1,"flag"] <- samFlags("READ_PAIRED")
		unfilteredReads <- as.data.frame(SamReads(sam2))
		name <- sam2$reads[1,"qname"]
		got <- pairedReads(sam2)
		filteredReads <- as.data.frame(SamReads(got))
		expect_equal( nrow(filteredReads), 2 )
		expect_equal( filteredReads[1,], unfilteredReads[3,] )
		expect_equal( filteredReads[2,], unfilteredReads[4,] )

		sam2 <- sam
		sam2$reads[4,"flag"] <- samFlags("READ_PAIRED")
		unfilteredReads <- as.data.frame(SamReads(sam2))
		name <- sam2$reads[4,"qname"]
		got <- pairedReads(sam2)
		filteredReads <- as.data.frame(SamReads(got))
		expect_equal( nrow(filteredReads), 2 )
		expect_equal( filteredReads[1,], unfilteredReads[1,] )
		expect_equal( filteredReads[2,], unfilteredReads[2,] )
	})
	describe( "Keeps only unpaired reads if paired=TRUE is set", {
		got <- pairedReads(sam, paired = FALSE)
		expect_equal(nrow(as.data.frame(SamReads(got))), 0)

		sam2 <- sam
		sam2$reads[1,"flag"] <- samFlags("READ_PAIRED")
		unfilteredReads <- as.data.frame(SamReads(sam2))
		name <- sam2$reads[1,"qname"]
		got <- pairedReads(sam2, paired = FALSE)
		filteredReads <- as.data.frame(SamReads(got))
		expect_equal( nrow(filteredReads), 2 )
		expect_equal( filteredReads[1,], unfilteredReads[1,] )
		expect_equal( filteredReads[2,], unfilteredReads[2,] )

		sam2 <- sam
		sam2$reads[4,"flag"] <- samFlags("READ_PAIRED")
		unfilteredReads <- as.data.frame(SamReads(sam2))
		name <- sam2$reads[4,"qname"]
		got <- pairedReads(sam2, paired = FALSE)
		filteredReads <- as.data.frame(SamReads(got))
		expect_equal( nrow(filteredReads), 2 )
		expect_equal( filteredReads[1,], unfilteredReads[3,] )
		expect_equal( filteredReads[2,], unfilteredReads[4,] )

	})
})
