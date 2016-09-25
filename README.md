
<!-- README.md is generated from README.Rmd. Please edit that file -->
SamSeq
======

Working with aligned reads from a `*.sam` file.

Allows loading `*.sam` files, i.e. the format used by SamTools. Tracks header information, reads, and the source of the file. Loads all reads and optionally the extra read tags into S3 objects that are also data frames. This allows easy manipulation and rquires much less overhead than using Bioconductor, but is significantly slower and uses more memory.

Includes functions to translate the Sam flags and to filter reads.

### Loading a sam file

A `Sam` object is created/constructed by the `Sam()` function, usually by passing it the name of a `*.sam` file as a parameter. This object is a list with a `SamHeader` and a `SamRead` object, and has a `SamSource` object as an attribute. These can be accessed directly but should use the accessor/creator methods of hte same name as the object classes. The folowing code uses a tiny sam file distributed with this package.

``` r
library(SamSeq)

# Get the name of a *. sam file to use in this example. If SamSeq is not
# installed in the normal place, specify the R library directory with
# the system.file parameter lib.loc=
sample.sam <- system.file( "extData/pe.sam", package = "SamSeq" )

samObj <- Sam( sample.sam )
#> [1] "Reading file: \"/Users/srj/Library/R/3.3/library/SamSeq/extData/pe.sam\". This may take a few seconds."
#> [1] "Loading basic read fields ..."
#> [1] "Converting optional tags to named list ..."
#> [1] "Binding optional tags as data frame ..."
#> [1] "Converting optional tags type ..."
class(samObj)
#> [1] "Sam"  "list"
names(samObj)
#> [1] "header" "reads"
names(attributes(samObj))
#> [1] "names"  "class"  "source"

# Header and Read objects are also data frames
samHeaderObj <- SamHeader( samObj )
class(samHeaderObj)
#> [1] "SamHeader"
samReadsObj <- SamReads( samObj )
class(samReadsObj)
#> [1] "SamReads"   "data.frame"
samSourceObj <- SamSource( samObj )
class(samSourceObj)
#> [1] "SamSource"
```
