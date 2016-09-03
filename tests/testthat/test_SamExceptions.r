context("Testing SamFileFormatExceptions and sub-classes.")

# Specific test fixture - test that a constructed IOexception has the correct
# content via all S3 accessors and for any additional data as well. Requires
# a list with the expected elements, see desc$ references below.
expect_SamFileFormatException_like <- function(ex, desc) {
	expect_equal( class( ex ), desc$class )
	expect_equalOrNull( conditionMessage( ex ), desc$displayMessage )
	expect_equalOrNull( conditionCall(    ex ), desc$call )
	expect_equalOrNull( exceptionPackage( ex ), desc$package )
	expect_equalOrNull( exceptionData(    ex ), desc$data )
	expect_equalOrNull( exceptionPath(    ex ), desc$path )
	expect_equalOrNull( exceptionLine(    ex ), desc$line )
	for( datum in names(desc$extData)) {
		expect_equal( ex[datum], desc$extData[datum] )
	}
}

samFileFormatExceptionDescription <- function(c) {
	list(
		class= c,
		rawMessage="A test message",
		displayMessage="[testMe] A test message",
		call= sys.calls(),
		package= "testMe",
		data= c(1,2,3),
		extData= list( A=1, B=list(a="amy", b="bob")),
		path= "/not/me.txt",
		line= 4242
	)
}

# Convert a list description of an exception to a list argument for use in a
# call("exceptionName").
samFileFormatExceptionDescriptionAsArguments <- function( desc ) {
	merge(
		list(
			message= desc$rawMessage,
			call= desc$call,
			package= desc$package,
			data= desc$data,
			path= desc$path,
			line= desc$line
		),
		desc$extData
	)
}

# To be definitive in testing.
thisPackage <- "SamSeq"

describe( 'Format string accessors generate correct messages', {
	it( "SamFileFormatMsg()", {
		got <- SamFileFormatMsg( line=123, path='FILE', wd='DIR', data='TEXT')
		want <- 	('A SamFileFormatException occurred at line 123 in sam file:'
					 %pp% '"FILE". (Running in: "DIR").\n'
					 %p% 'That line began: "TEXT".')
		expect_equal(got, want)
	})
	it( "SamFileHeaderMsg()", {
		got <- SamFileHeaderMsg( line=123, path='FILE', wd='DIR', data='TEXT')
		want <- ('A SamFileHeaderException occurred parsing the sam file header'
					%pp% 'at line 123 in file: "FILE". (Running in: "DIR").\n'
					%p% 'That line began: "TEXT".')
		expect_equal(got, want)
	})
	it( "SamFileRead_ISSS()", {
		got <- SamFileReadMsg( line= 123, path= 'FILE', wd= 'DIR', data= 'TEXT' )
		want <- ('A SamFileReadException occurred parsing the sam file read'
					%pp% 'at line 123 in file: "FILE". (Running in: "DIR").\n'
					%p% 'That line began: "TEXT".')
		expect_equal(got, want)
	})
	it( "MISSING_HEADER_Msg()", {
		got <- MISSING_HEADER_Msg( path="FILE", wd="DIR" )
		want <- ('A MISSING_HEADER Exception occurred in sam file: "FILE".'
					%pp% '(Running in: "DIR").\n'
					%p% 'Sam files require headers.'
					%pp% 'Temporary files with just reads are not true sam files.')
		expect_equal(got, want)
	})
	it( "HeaderOnlyMsg()", {
		got <- HeaderOnlyMsg( path="FILE", wd="DIR" )
		want <- ('A HeaderOnlyException occurred in sam file: "FILE".'
					%pp% '(Running in: "DIR").\n'
					%p% 'Sam files require reads.'
					%pp% 'Temporary files with just headers are not true sam files.')
		expect_equal(got, want)
	})

})

describe( "SamFileFormatException", {
	SamFileFormatExceptionExceptionClasses = c( "SamFileFormatException",
		"FileFormatException", "DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- SamFileFormatException( path="../" )
		desc <- list(
			class=    SamFileFormatExceptionExceptionClasses,
			displayMessage= (
				'[' %p% thisPackage %p% ']' %pp% SamFileFormatMsg( path="../" )
			),
			call= NULL, package=  thisPackage,
			path= "../", data=NA, line=NA
		)
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- samFileFormatExceptionDescription( SamFileFormatExceptionExceptionClasses )
		argList <- samFileFormatExceptionDescriptionAsArguments( desc )
		got <- do.call( "SamFileFormatException", argList )
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( SamFileFormatException(package= NULL, path=NA ))
		want= SamFileFormatMsg()
		expect_equal( got, want )
		got <- conditionMessage( SamFileFormatException(
			message= "new message", package=NULL, path=NA ))
		want <- "new message"
		expect_equal( got, want )
	})
})

describe( "SamFileReadException", {
	SamFileReadExceptionExceptionClasses = c( "SamFileReadException",
		"SamFileFormatException", "FileFormatException", "DataException",
		"Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- SamFileReadException( path="../" )
		desc <- list(
			class=    SamFileReadExceptionExceptionClasses,
			displayMessage= (
				'[' %p% thisPackage %p% ']'
				%pp% SamFileReadMsg(path="../")
			),
			call= NULL, package=  thisPackage,
			path= "../", data=NA, line=NA
		)
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- samFileFormatExceptionDescription( SamFileReadExceptionExceptionClasses )
		argList <- samFileFormatExceptionDescriptionAsArguments( desc )
		got <- do.call( "SamFileReadException", argList )
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( SamFileReadException(package= NULL, path=NA ))
		want= SamFileReadMsg()
		expect_equal( got, want )
		got <- conditionMessage( SamFileReadException(
			message= "new message", package=NULL, path=NA ))
		want <- "new message"
		expect_equal( got, want )
	})
})

describe( "SamFileHeaderException", {
	SamFileHeaderExceptionExceptionClasses = c( "SamFileHeaderException",
		"SamFileFormatException", "FileFormatException", "DataException",
		"Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- SamFileHeaderException( path="../" )
		desc <- list(
			class=    SamFileHeaderExceptionExceptionClasses,
			displayMessage= (
				'[' %p% thisPackage %p% ']'
				%pp% SamFileHeaderMsg(path= '../')
			), call= NULL, package=  thisPackage,
			path= "../", data=NA, line=NA
		)
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- samFileFormatExceptionDescription( SamFileHeaderExceptionExceptionClasses )
		argList <- samFileFormatExceptionDescriptionAsArguments( desc )
		got <- do.call( "SamFileHeaderException", argList )
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( SamFileHeaderException(package= NULL, path=NA ))
		want= SamFileHeaderMsg()
		expect_equal( got, want )
		got <- conditionMessage( SamFileHeaderException(
			message= "new message", package=NULL, path=NA ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "MISSING_HEADER_Exception", {
	MISSING_HEADER_ExceptionClasses = c( "MISSING_HEADER_Exception",
		"SamFileHeaderException", "SamFileFormatException", "FileFormatException",
		"DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- MISSING_HEADER_Exception( path="../" )
		desc <- list(
			class=    MISSING_HEADER_ExceptionClasses,
			displayMessage= (
				'[' %p% thisPackage %p% ']'
            %pp% MISSING_HEADER_Msg( path= "../" )
			), call= NULL, package=  thisPackage,
			path= "../", data=NA, line=NA
		)
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- samFileFormatExceptionDescription( MISSING_HEADER_ExceptionClasses )
		argList <- samFileFormatExceptionDescriptionAsArguments( desc )
		got <- do.call( "MISSING_HEADER_Exception", argList )
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( MISSING_HEADER_Exception(package= NULL, path=NA ))
		want= MISSING_HEADER_Msg()
		expect_equal( got, want )
		got <- conditionMessage( MISSING_HEADER_Exception(
			message= "new message", package=NULL, path=NA ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "HeaderOnlyException", {
	HeaderOnlyExceptionClasses = c( "HeaderOnlyException",
		"SamFileReadException", "SamFileFormatException", "FileFormatException",
		"DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- HeaderOnlyException( path="../" )
		desc <- list(
			class= HeaderOnlyExceptionClasses,
			displayMessage= '[' %p% thisPackage %p% ']' %pp% HeaderOnlyMsg( path= "../" ),
			call= NULL, package=  thisPackage,
			path= "../", data=NA, line=NA
		)
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- samFileFormatExceptionDescription( HeaderOnlyExceptionClasses )
		argList <- samFileFormatExceptionDescriptionAsArguments( desc )
		got <- do.call( "HeaderOnlyException", argList )
		expect_SamFileFormatException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( HeaderOnlyException(package= NULL, path=NA ))
		want= HeaderOnlyMsg()
		expect_equal( got, want )
		got <- conditionMessage( HeaderOnlyException(
			message= "new message", package=NULL, path=NA ))
		want <- "new message"
		expect_equal( got, want )
	})
})
