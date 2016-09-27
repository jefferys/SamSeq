
<!-- README.md is generated from README.Rmd. Please edit that file -->
SamSeq
======

Allows loading and working with `*.sam` files, i.e. the format used by SamTools. Tracks header information, reads, and the source of the file. Loads all reads and optionally the extra read tags into S3 objects that are also data frames. This allows easy manipulation and rquires much less overhead than using Bioconductor, but is significantly slower and uses more memory.

Includes functions to translate the Sam flags and to filter reads.

### Loading a sam file

A `Sam` object is created/constructed by the `Sam()` function, usually by passing it the name of a `*.sam` file as a parameter. From this object, the sam header data can be accessed with `SamHeader()`, the sam read data with `SamRead` and the source information about the sam file with `SamSource()`. The folowing code uses a tiny sam file distributed with this package.

``` r
library(SamSeq)

# Get the name of a *. sam file to use in this example. If SamSeq is not
# installed in the normal place, specify the R library directory with
# the system.file parameter lib.loc=
sample.sam <- system.file( "extData/pe.sam", package = "SamSeq" )

samObj <- Sam( sample.sam )
#> [1] "Reading file:"                                                       
#> [2] "    \"/Users/srj/Library/R/3.3/library/SamSeq/extData/pe.sam\". This"
#> [3] "    may take a few seconds."                                         
#> [1] "Loading basic read fields ..."
#> [1] "Converting optional tags to named list ..."
#> [1] "Binding optional tags as data frame ..."
#> [1] "Converting optional tags type ..."
class(samObj)
#> [1] "Sam"  "list"

samHeaderObj <- SamHeader( samObj )
class(samHeaderObj)
#> [1] "SamHeader"
samReadsObj <- SamReads( samObj )
class(samReadsObj)
#> [1] "SamReads"
samSourceObj <- SamSource( samObj )
class(samSourceObj)
#> [1] "SamSource" "list"
```

### SamHeader objects

The SamHeader is not fully parsed at this point, it will probably have a different structure in future. For now it is provided as an object that can be cast to a data frame with two columns: the initial tag as the column `tag`, and the rest of the line as the column `record`.

``` r
samHeaderObj <- SamHeader( samObj )
headerDF <- as.data.frame(samHeaderObj)

names(headerDF)
#> [1] "tag"    "record"
headerDF[1, ]
#>   tag record
#> 1  HD VN:1.4
headerDF$tag
#> [1] "HD" "SQ" "SQ" "SQ" "PG" "CO"
headerDF[headerDF$tag == "SQ", "record" ]
#> [1] "SN:chr7\tLN:159138663"                
#> [2] "SN:chr22\tLN:51304566"                
#> [3] "SN:gi|333031|lcl|HPV16REF.1|\tLN:7906"

areHeaderTagsParsed(samHeaderObj)
#> [1] FALSE
areHeaderTagsParsed(samHeaderObj) == areHeaderTagsParsed(samObj)
#> [1] TRUE
```

The problem yet to be determined is how to split the contents when some header lines use tabs for delimiters and others can contain embedded unquoted tabs. Probably some form of parameter object that will allow specifying, on a per tag basis, what to do.

Vectors of raw header lines can also be parsed directly into a `SamHeader` object. Source information can be provided, by default it will all be `NA`.

``` r
headerLines <- c(
    "@HD\tVN:1.4",
    "@SQ\tSN:chr7\tLN:159138663"
)
headersObj <- SamHeader( headerLines )
headerDF <- as.data.frame(samHeaderObj)
headerDF[1:2, ]
#>   tag                record
#> 1  HD                VN:1.4
#> 2  SQ SN:chr7\tLN:159138663
```

### SamReads objects

The reads in a `*.sam` file are represented as a `SamReads` object, which is a data frame containing the fixed 11 columns of read data with the same name as the sam fields, plus an additional column or columns containing the optional read tags. By default the optional read tags are parsed out into columns with the tag as the column name and the value converted to integer or double if appropriate, otherwise left as character.

``` r
samReadsObj <- SamReads( samObj )
readsDF <- as.data.frame(samReadsObj)

names(readsDF)
#>  [1] "qname" "flag"  "rname" "pos"   "mapq"  "cigar" "rnext" "pnext"
#>  [9] "tlen"  "seq"   "qual"  "NH"    "HI"    "AS"    "nM"    "xT"
readsDF[1:2, ]
#>                                      qname flag rname      pos mapq cigar
#> 1 D7T4KXP1:397:C5F8LACXX:8:2106:4860:19981  113  chr7 73566935    3   48M
#> 2 D7T4KXP1:397:C5F8LACXX:8:2106:4860:19981  177  chr7 73566935    3   48M
#>   rnext    pnext tlen                                              seq
#> 1  chr7 73566935    0 ATGGCGCGATCTCGGCTCACTGCAACCTCCGCCTCCTGGGTGCAAGCG
#> 2  chr7 73566935    0 ATGGCGCGATCTCGGCTCACTGCAACCTCCGCCTCCTGGGTGCAAGCG
#>                                               qual NH HI AS nM      xT
#> 1 DDBDDFFEHHJGIGIGHHIJJIHFFGIIEGGIJJJHHHHHFFFFDC@@  2  1 45  1    <NA>
#> 2 DDBDDFFEHHJGIGIGHHIJJIHFFGIIEGGIJJJHHHHHFFFFDC@@  2  2 45  1 Test Me
readsDF$flag
#> [1] 113 177  81 161

areReadTagsParsed(samReadsObj)
#> [1] TRUE
areReadTagsParsed(samReadsObj) == areReadTagsParsed(samObj)
#> [1] TRUE
```

If `splitTags= TRU`E is set when reading in the file with `Sam()`, tags will be left as a single unparsed `tags` column with values the uparsed tags delimited by embedded tabs. You might want to leave the optional tags unparsed if you don't need them as parsing them makes reading the file 4 times slower.

``` r
rawTagsSamObj <- Sam( sample.sam, splitTags = FALSE )
#> [1] "Reading file:"                                                       
#> [2] "    \"/Users/srj/Library/R/3.3/library/SamSeq/extData/pe.sam\". This"
#> [3] "    may take a few seconds."                                         
#> [1] "Loading basic read fields ..."
rawTagsReadObj <- SamReads( rawTagsSamObj )
rawReadsDf <- as.data.frame(rawTagsReadObj)

names(rawReadsDf)
#>  [1] "qname" "flag"  "rname" "pos"   "mapq"  "cigar" "rnext" "pnext"
#>  [9] "tlen"  "seq"   "qual"  "tags"
rawReadsDf[1:2, ]
#>                                      qname flag rname      pos mapq cigar
#> 1 D7T4KXP1:397:C5F8LACXX:8:2106:4860:19981  113  chr7 73566935    3   48M
#> 2 D7T4KXP1:397:C5F8LACXX:8:2106:4860:19981  177  chr7 73566935    3   48M
#>   rnext    pnext tlen                                              seq
#> 1  chr7 73566935    0 ATGGCGCGATCTCGGCTCACTGCAACCTCCGCCTCCTGGGTGCAAGCG
#> 2  chr7 73566935    0 ATGGCGCGATCTCGGCTCACTGCAACCTCCGCCTCCTGGGTGCAAGCG
#>                                               qual
#> 1 DDBDDFFEHHJGIGIGHHIJJIHFFGIIEGGIJJJHHHHHFFFFDC@@
#> 2 DDBDDFFEHHJGIGIGHHIJJIHFFGIIEGGIJJJHHHHHFFFFDC@@
#>                                            tags
#> 1               NH:i:2\tHI:i:1\tAS:i:45\tnM:i:1
#> 2 NH:i:2\tHI:i:2\tAS:i:45\tnM:i:1\txT:Z:Test Me
rawReadsDf$tags
#> [1] "NH:i:2\tHI:i:1\tAS:i:45\tnM:i:1"              
#> [2] "NH:i:2\tHI:i:2\tAS:i:45\tnM:i:1\txT:Z:Test Me"
#> [3] "NH:i:2\tHI:i:1\tAS:i:47\tnM:i:0"              
#> [4] "NH:i:2\tHI:i:1\tAS:i:47\tnM:i:0"

areReadTagsParsed(rawTagsReadObj)
#> [1] FALSE
areReadTagsParsed(rawTagsReadObj) == areReadTagsParsed(rawTagsSamObj)
#> [1] TRUE
```

### SamSource objects

The source of the sam data is automatically bound to `Sam` object created when the file is read in, and propagated to the `SamReads` and `SamHeader` objects. It can be obtained by the `SamSource()` accessor.

``` r
samFileSourceObj <- SamSource( samObj )
samHeaderSourceObj <- SamSource( SamHeader( samObj ))
samReadsSourceObj <- SamSource( SamReads( samObj ))
identical( samFileSourceObj, samHeaderSourceObj)
#> [1] TRUE
identical( samFileSourceObj, samReadsSourceObj)
#> [1] TRUE
```

A SamSource object has accessors `samSourceName()`, `samSourceHost()`, and `samSourceType()` which return the name of the file read, the name of machine with the file, and the type of access used to get the data, respectively. Currently the only type of access is `file`. A possible future type might be `url`.

The accessors can be applied directly to the `Sam`, `SamHeader`, and `SamReads` objects, or indeed any object for which SamSource(x) is defined.

``` r
samSource <- SamSource( samObj )
samSourceName( samSource )
#> [1] "/Users/srj/Library/R/3.3/library/SamSeq/extData/pe.sam"
samSourceHost( samSource )
#> [1] "srj-at-unc-dot-edu.local"
samSourceType( samSource )
#> [1] "file"
```

The accessors are S3 functions, and can be applied directly to the `Sam`, `SamHeader`, and `SamReads` objects without first extracting the `SamSource` object.

``` r
samSourceName( samObj )
#> [1] "/Users/srj/Library/R/3.3/library/SamSeq/extData/pe.sam"
samSourceHost( samHeaderObj )
#> [1] "srj-at-unc-dot-edu.local"
samSourceType( samReadsObj )
#> [1] "file"
```

### Sam Flags

To be detailed - see `?samFlags` and `?testSamFlags`

### Sam Filters

To be detailed - see `?pairedReads`, `?samReadFilter`, `?pairedEndReadTests`, and `?readTests`.
