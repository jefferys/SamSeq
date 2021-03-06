context("Testing the Sam file object functionality")

noSuchFile <- tempfile("noSuchFile.sam")
emptyFile <- tempfile("emptyFile.sam")
file.create(emptyFile)

samFile <- file.path( "goodSamFiles", "pe.sam" )
package <- packageName()

describe( "Test fixtures for use in the test_sam testthat file", {
   describe( "Files and directories provided for testing", {
      it( "provides 'noSuchFile' - A file path that does not exist.", {
         expect_true( ! file.exists( noSuchFile ))
      })
   	it( "provides 'empty' - A file of size 0.", {
   		expect_true( file.exists( emptyFile ))
   		expect_equal( file.size( emptyFile ), 0L)
   	})
   	it( "Provides 'samFile' as a generic input sam file.", {
   		expect_true( file.exists( samFile ))
   	})
   })
})

describe( "Loading a simple file from disk", {
	describe( "Errors that prevent loading data", {
	   it( "Errors if the sam file does not exist", {
	   	expect_condition( Sam(noSuchFile), "NoSuchFileException" )
	   })
		it( "Errors if the sam file is empty", {
			expect_condition( capture_output(Sam(emptyFile)), "EmptyFileException" )
		})
	})
   describe( "Sam object structure.", {

   	expect_output( sam <- Sam(samFile) )
      it( "Is a sam object", {
      	expect_s3_class(sam, "Sam")
      })
      expect_output( sam <- Sam(samFile, splitTags = FALSE) )
      it( "Is a sam object", {
      	expect_s3_class(sam, "Sam")
      })
   })
})
