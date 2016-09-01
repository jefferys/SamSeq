context("Testing DataExceptions and sub-classes.")

# Specific test fixture - test that a constructed IOexception has the correct
# content via all S3 accessors and for any additional data as well. Requires
# a list with the expected elements, see desc$ references below.
expect_DataException_like <- function(ex, desc) {
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

dataExceptionDescription <- function(c) {
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
dataExceptionDescriptionAsArguments <- function( desc ) {
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

describe( "DataException", {
	DataExceptionClasses = c( "DataException", "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- DataException(data=NULL)
		desc <- list(
			class=    DataExceptionClasses,
			displayMessage= sprintf("[%s] A DataException occurred.", thisPackage),
			call=     NULL,
			package=  thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( DataExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "DataException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( DataException(package= NULL, data=NULL ))
		want <- "A DataException occurred."
		expect_equal( got, want )
		got <- conditionMessage( DataException(message= "new message", package= NULL, data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})
describe( "FileFormatException", {
	FileFormatExceptionClasses = c( "FileFormatException","DataException",
											  "Exception", "condition" )
	it( "Constructs the expected default object", {
		got <- FileFormatException(path=NA_character_)
		desc <- list(
			class=    FileFormatExceptionClasses,
			displayMessage= ('[' %p% thisPackage %p% '] A FileFormatException occurred at line'
				%pp% 'NA in file: "NA". (Running in: "' %p% getwd()
			   %p% '").\nThat line began: "NA".'),
			call=     NULL,
			path= NA_character_,
			data= NA_character_,
			line= NA_integer_,
			package=  thisPackage
		)
		expect_DataException_like( got, desc )
	})
	it( "Can set message, call, package, data, and extData", {
		desc <- dataExceptionDescription( FileFormatExceptionClasses )
		argList <- dataExceptionDescriptionAsArguments( desc )
		got <- do.call( "FileFormatException", argList )
		expect_DataException_like( got, desc )
	})
	it( "Displays raw message if package is null", {
		got <- conditionMessage( FileFormatException(path=NA))
		want <- ('[' %p% thisPackage %p% '] A FileFormatException occurred at line'
					%pp% 'NA in file: "NA". (Running in: "' %p% getwd()
					%p% '").\nThat line began: "NA".')
		expect_equal( got, want )
		got <- conditionMessage( FileFormatException(
			message= "new message", package= NULL, path="bob", data=42 ))
		want <- "new message"
		expect_equal( got, want )
	})
})
