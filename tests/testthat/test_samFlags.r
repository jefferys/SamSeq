context("Testing sam flag coding and decoding")

flagCount <- 12

describe( "samFlag()", {
	describe( "By default provides flag names, values, and descriptions.", {
		expect_silent( flagDat <- samFlag() );

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
			expect_equal( samFlag("READ_PAIRED"), 1 )
			expect_equal( samFlag("PROPER_PAIR"), 2 )
			expect_equal( samFlag("SUPPLEMENTARY_ALIGNMENT"), 2^11 )
		})
		it( "Returs the sum of the flag's value if more than one flag, specified as 1 vector.", {
			expect_equal( samFlag(c("READ_PAIRED", "PROPER_PAIR", "SUPPLEMENTARY_ALIGNMENT")),
							  1 + 2 + 2^11 )
			expect_equal( samFlag(c("READ_PAIRED", "PROPER_PAIR")), 3 )
		})
		it( "Returs the sum of the flag's value if more than one flag, specified as arguments.", {
			expect_equal( samFlag("READ_PAIRED", "PROPER_PAIR", "SUPPLEMENTARY_ALIGNMENT"),
							  1 + 2 + 2^11 )
			expect_equal( samFlag("READ_PAIRED", "PROPER_PAIR"), 3 )
		})
		it( "returns 0 if no flag specified", {
			expect_equal( samFlag( character(0) ), 0 )
		})
		it( "returns NA if any flag incorrectly specified", {
			expect_true( is.na(samFlag( c("BOB") )))
			expect_true( is.na(samFlag( c("READ_PAIRED", "BOB") )))
			expect_true( is.na(samFlag("BOB" )))
			expect_true( is.na(samFlag("READ_PAIRED", "BOB" )))
		})
	})
	describe( "Returns a logical vector corresponding to a specified flag value.", {
		it( "Works when only one flag should be true", {
			got <- samFlag(1)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("READ_PAIRED"=12))
			got <- samFlag(2)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("PROPER_PAIR"=11))
			got <- samFlag( 2^11 )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("SUPPLEMENTARY_ALIGNMENT" = 1))
		})
		it( "Works when multiple flags are true", {
			got <- samFlag(3)
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("PROPER_PAIR"=11, "READ_PAIRED"=12))
			got <- samFlag( 1+2+2^11 )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got),
					c("SUPPLEMENTARY_ALIGNMENT" = 1, "PROPER_PAIR"=11, "READ_PAIRED"=12))
		})
		it( "Works for out of bounds values", {
			samFlagVal <- 2^(flagCount + 1)
			expect_false( any( samFlag( samFlagVal )))

			samFlagVal <- 2^(flagCount + 1) + 1
			got <- samFlag( samFlagVal )
			expect_equal( length(got), flagCount )
			expect_true( is.logical(got) )
			expect_equal( which(got), c("READ_PAIRED"=12))

		})
		it( "Works for no flags", {
			expect_false( any(samFlag(0)))
		})
	})
})
