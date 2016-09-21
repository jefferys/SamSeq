context("Testing sam flag coding and decoding")

# Total number of flags
flagCount <- 12

# Value if just last flag set
lastFlag <- 2^(flagCount - 1)

# Value if all flags set
allFlags <- 2^flagCount - 1

describe( "samFlags()", {
	describe( "By default provides flag names, values, and descriptions.", {
		expect_silent( flagDat <- samFlags() );

		it( "Provides a data frame describing (flagCount) flags", {
			expect_true( is.data.frame(flagDat), "Wanted a data frame" )
			expect_equal( nrow(flagDat), flagCount )
		})
		it( "Provides flag names as row names", {
			flags <- row.names(flagDat)
			expect_equal( length(flags), flagCount)
			expect_gt(max(nchar(flags)), 0)
			expect_equal( anyDuplicated(flags), 0 )
		})
		it( "Provides values as power of 2 integers", {
			expect_equal( 2^(0:(flagCount - 1)), flagDat$value)
		})
	})
	describe( "Returns the flag value corresponding to specified flag names", {
		it( "Returs the flag's value if just one flag.", {
			expect_equal( samFlags("READ_PAIRED"), 1 )
			expect_equal( samFlags("PROPER_PAIR"), 2 )
			expect_equal( samFlags("SUPPLEMENTARY_ALIGNMENT"), lastFlag )
		})
		it( "Returs the sum of the flag's value if more than one flag, specified as 1 vector.", {
			expect_equal( samFlags(c("READ_PAIRED", "PROPER_PAIR", "SUPPLEMENTARY_ALIGNMENT")),
							  1 + 2 + lastFlag )
			expect_equal( samFlags(c("READ_PAIRED", "PROPER_PAIR")), 3 )
		})
		it( "Returs the sum of the flag's value if more than one flag, specified as arguments.", {
			expect_equal( samFlags("READ_PAIRED", "PROPER_PAIR", "SUPPLEMENTARY_ALIGNMENT"),
							  1 + 2 + lastFlag )
			expect_equal( samFlags("READ_PAIRED", "PROPER_PAIR"), 3 )
		})
		it( "returns 0 if no flag specified", {
			expect_equal( samFlags( character(0) ), 0 )
		})
		it( "returns NA if any flag incorrectly specified", {
			expect_true( is.na(samFlags( c("BOB") )))
			expect_true( is.na(samFlags( c("READ_PAIRED", "BOB") )))
			expect_true( is.na(samFlags("BOB" )))
			expect_true( is.na(samFlags("READ_PAIRED", "BOB" )))
		})
	})
	describe( "Returns a logical vector corresponding to a specified flag value.", {
		it( "Works when only one flag should be true", {
			got <- samFlags(1)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("READ_PAIRED"=12))
			got <- samFlags(2)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("PROPER_PAIR"=11))
			got <- samFlags( lastFlag )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("SUPPLEMENTARY_ALIGNMENT" = 1))
		})
		it( "Works when multiple flags are true", {
			got <- samFlags(3)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("PROPER_PAIR"=11, "READ_PAIRED"=12))
			got <- samFlags( 1+2+lastFlag )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got),
					c("SUPPLEMENTARY_ALIGNMENT" = 1, "PROPER_PAIR"=11, "READ_PAIRED"=12))
		})
		it( "Works for out of bounds values", {
			samFlagVal <- 2^(flagCount + 1)
			expect_false( any( samFlags( samFlagVal )))

			samFlagVal <- 2^(flagCount + 1) + 1
			got <- samFlags( samFlagVal )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("READ_PAIRED"=12))

		})
		it( "Works for no flags", {
			expect_false( any(samFlags(0)))
		})
	})
})
describe( "allSetSamFlags()", {
	describe( "Is TRUE if all set, whether or not some unspecified are set", {
		it ("works for single (set) flags", {
			expect_true( allSetSamFlags( 1,   "READ_PAIRED" ))
			expect_true( allSetSamFlags( 1+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all set) flags, regardless of order", {
			expect_true( allSetSamFlags( 1+2,   c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( allSetSamFlags( 1+2,   c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( allSetSamFlags( 1+2+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( allSetSamFlags( 1+2+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
	describe( "Is FALSE if any unset, whether or not some unspecified are set", {
		it ("works for single unset flags", {
			expect_false( allSetSamFlags( 0, "READ_PAIRED" ))
			expect_false( allSetSamFlags( 0+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all unset) flags, regardless of order", {
			expect_false( allSetSamFlags( 0, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allSetSamFlags( 0, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( allSetSamFlags( 4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allSetSamFlags( 4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
		it ("works for multiple (mixed unset/set) flags, regardless of order", {
			expect_false( allSetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allSetSamFlags( 1, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( allSetSamFlags( 1+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allSetSamFlags( 1+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
})
describe( "anySetSamFlags()", {
	describe( "Is TRUE if any set, whether or not some unspecified are set", {
		it ("works for single set flags", {
			expect_true( anySetSamFlags( 1, "READ_PAIRED" ))
			expect_true( anySetSamFlags( 1+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all set) flags, regardless of order", {
			expect_true( anySetSamFlags( 1+2, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anySetSamFlags( 1+2, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( anySetSamFlags( 1+2+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anySetSamFlags( 1+2+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
		it ("works for multiple (mixed unset/set) flags, regardless of order", {
			expect_true( anySetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anySetSamFlags( 1, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( anySetSamFlags( 1+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anySetSamFlags( 1+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
	describe( "Is FALSE if all unset, whether or not some unspecified are set", {
		it ("works for single (set) flags", {
			expect_false( anySetSamFlags( 0,   "READ_PAIRED" ))
			expect_false( anySetSamFlags( 0+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all unset) flags, regardless of order", {
			expect_false( anySetSamFlags( 0,   c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( anySetSamFlags( 0,   c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( anySetSamFlags( 0+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( anySetSamFlags( 0+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
})
describe( "anyUnsetSamFlags()", {
	describe( "Is TRUE if any unset, whether or not some unspecified are set", {
		it ("works for single unset flags", {
			expect_true( anyUnsetSamFlags( 0, "READ_PAIRED" ))
			expect_true( anyUnsetSamFlags( 0+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all unset) flags, regardless of order", {
			expect_true( anyUnsetSamFlags( 0, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anyUnsetSamFlags( 0, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( anyUnsetSamFlags( 4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anyUnsetSamFlags( 4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
		it ("works for multiple (mixed unset/set) flags, regardless of order", {
			expect_true( anyUnsetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anyUnsetSamFlags( 1, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( anyUnsetSamFlags( 1+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( anyUnsetSamFlags( 1+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
	describe( "Is FALSE if all set, whether or not some unspecified are set", {
		it ("works for single (set) flags", {
			expect_false( anyUnsetSamFlags( 1,   "READ_PAIRED" ))
			expect_false( anyUnsetSamFlags( 1+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all set) flags, regardless of order", {
			expect_false( anyUnsetSamFlags( 1+2,   c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( anyUnsetSamFlags( 1+2,   c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( anyUnsetSamFlags( 1+2+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( anyUnsetSamFlags( 1+2+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
})
describe( "allUnsetSamFlags()", {
	describe( "Is TRUE if all unset, whether or not some unspecified are set", {
		it ("works for single (set) flags", {
			expect_true( allUnsetSamFlags( 0,   "READ_PAIRED" ))
			expect_true( allUnsetSamFlags( 0+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all unset) flags, regardless of order", {
			expect_true( allUnsetSamFlags( 0,   c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( allUnsetSamFlags( 0,   c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_true( allUnsetSamFlags( 0+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_true( allUnsetSamFlags( 0+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
	describe( "Is FALSE if any set, whether or not some unspecified are set", {
		it ("works for single set flags", {
			expect_false( allUnsetSamFlags( 1, "READ_PAIRED" ))
			expect_false( allUnsetSamFlags( 1+2, "READ_PAIRED" ))
		})
		it ("works for multiple (all set) flags, regardless of order", {
			expect_false( allUnsetSamFlags( 1+2, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allUnsetSamFlags( 1+2, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( allUnsetSamFlags( 1+2+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allUnsetSamFlags( 1+2+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
		it ("works for multiple (mixed unset/set) flags, regardless of order", {
			expect_false( allUnsetSamFlags( 1, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allUnsetSamFlags( 1, c( "PROPER_PAIR", "READ_PAIRED" )))
			expect_false( allUnsetSamFlags( 1+4, c( "READ_PAIRED", "PROPER_PAIR" )))
			expect_false( allUnsetSamFlags( 1+4, c( "PROPER_PAIR", "READ_PAIRED" )))
		})
	})
})
describe( "matchSamFlags()", {
	it( "Works with one T flag in flag vector", {
		flagVec <- c("READ_PAIRED" = TRUE)
		expect_true(matchSamFlags(1, flagVec))
		expect_false(matchSamFlags(2, flagVec))
		expect_true(matchSamFlags(1+2, flagVec))
	})
	it( "Matching with one F flag in flag vector", {
		flagVec <- c("READ_PAIRED" = FALSE)
		expect_false(matchSamFlags(1, flagVec))
		expect_true(matchSamFlags(2, flagVec))
		expect_false(matchSamFlags(1+2, flagVec))
	})
	it( "Matching with two T flags in flag vector", {
		flagVec <- c("READ_PAIRED" = TRUE, "PROPER_PAIR" = TRUE)
		expect_false(matchSamFlags(1, flagVec))
		expect_false(matchSamFlags(2, flagVec))
		expect_true(matchSamFlags(1+2, flagVec))
		expect_false(matchSamFlags(4, flagVec))
		expect_false(matchSamFlags(1+4, flagVec))
		expect_false(matchSamFlags(2+4, flagVec))
		expect_true(matchSamFlags(1+2+4, flagVec))
	})
	it( "Matching with two F flags in flag vector", {
		flagVec <- c("READ_PAIRED" = FALSE, "PROPER_PAIR" = FALSE)
		expect_false(matchSamFlags(1, flagVec))
		expect_false(matchSamFlags(2, flagVec))
		expect_false(matchSamFlags(1+2, flagVec))
		expect_true(matchSamFlags(4, flagVec))
		expect_false(matchSamFlags(1+4, flagVec))
		expect_false(matchSamFlags(2+4, flagVec))
		expect_false(matchSamFlags(1+2+4, flagVec))
	})
	it( "Matching with one T and one F flag in flag vector", {
		flagVec <- c("READ_PAIRED" = FALSE, "PROPER_PAIR" = TRUE)
		expect_false(matchSamFlags(1, flagVec))
		expect_true(matchSamFlags(2, flagVec))
		expect_false(matchSamFlags(1+2, flagVec))
		expect_false(matchSamFlags(4, flagVec))
		expect_false(matchSamFlags(1+4, flagVec))
		expect_true(matchSamFlags(2+4, flagVec))
		expect_false(matchSamFlags(1+2+4, flagVec))
	})
	it( "Matching with two T and two F flags in flag vector", {
		flagVec <- c("READ_PAIRED" = FALSE, "PROPER_PAIR" = TRUE,
						 "READ_UNMAPPED" =  FALSE, "MATE_UNMAPPED" = TRUE)
		expect_false(matchSamFlags(1, flagVec))
		expect_false(matchSamFlags(2, flagVec))
		expect_false(matchSamFlags(1+2, flagVec))
		expect_false(matchSamFlags(4, flagVec))
		expect_false(matchSamFlags(1+4, flagVec))
		expect_false(matchSamFlags(2+4, flagVec))
		expect_false(matchSamFlags(1+2+4, flagVec))
		expect_false(matchSamFlags(8, flagVec))
		expect_false(matchSamFlags(1+8, flagVec))
		expect_true(matchSamFlags(2+8, flagVec))
		expect_false(matchSamFlags(1+2+8, flagVec))
		expect_false(matchSamFlags(4+8, flagVec))
		expect_false(matchSamFlags(1+4+8, flagVec))
		expect_false(matchSamFlags(2+4+8, flagVec))
		expect_false(matchSamFlags(1+2+4+8, flagVec))
		expect_true(matchSamFlags(2+8+16, flagVec))
		expect_false(matchSamFlags(16, flagVec))
		expect_false(matchSamFlags(2+16, flagVec))
		expect_false(matchSamFlags(8+16, flagVec))
		expect_false(matchSamFlags(1+2+8+16, flagVec))
	})
})
