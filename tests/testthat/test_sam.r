context("Testing the Sam file object functionality")

noSuchFile <- tempfile("noSuchFile.sam")
samFile <- file.path( "goodSamFiles", "pe.sam" )
package <- packageName()

describe( "Test fixtures for use in the test_sam testthat file", {
   describe( "'noSuchFile' - A file path that does not exist.", {
      it( "Fails to exist", {
         expect_true( ! file.exists( noSuchFile ))
      })
   })
})

describe( "Loading a sam file from disk", {
   it( "Errors if the sam file does not exist", {
      expect_condition( Sam(noSuchFile), "NoSuchFileException" )
   })
   describe( "Sam object structure.", {
      sam <- Sam(samFile)
      it( "Is a sam object", {
         expect_s3_class(sam, "Sam")
      })

      it( "Can retrieve a header, a reads and a meta object", {
         header <- SamHeader(sam)
         expect_s3_class(header, "SamHeader")
         reads <- SamReads(sam)
         expect_s3_class(reads, "SamReads")
         meta <- SamMeta(sam)
         expect_s3_class(meta, "SamMeta")
      })
   })
})
