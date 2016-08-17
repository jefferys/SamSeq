context( "Testing exceptions." )

describe( "condition() object constructor", {

   defaultCondition <- condition()
   messageCondition <- condition( message= "a message" )

   aCallStack <- sys.calls()
   callCondition <- condition( message="Now with calls.", call= aCallStack)

   it( "generates  'condition' class object", {
      expect_equal( class( defaultCondition ), 'condition' )
      expect_equal( class( messageCondition ), 'condition' )
      expect_equal( class(    callCondition ), 'condition' )
   })
   it( "generates objects with the expected message content", {
      expect_equal( conditionMessage( defaultCondition ),       'condition' )
      expect_equal( conditionMessage( messageCondition ),       'a message' )
      expect_equal( conditionMessage(    callCondition ), 'Now with calls.' )
   })
   it( "generates objects with the expected calls content", {
      expect_null(  conditionCall( defaultCondition ))
      expect_null(  conditionCall( messageCondition ))
      expect_equal( conditionCall( callCondition ), aCallStack )
   })
})

describe( "Extending an exception with extendException()", {
   it( "Extends the correct base class", {
      e <- extendException( "NewException", base=condition() )
      got <- class( e )
      want <- c("NewException", "condition")
      expect_equal( got, want )

      ee <- extendException( "newNewException", base= e )
      got <- class( ee )
      want <- c("newNewException", "NewException", "condition")
      expect_equal( got, want )

      default <- extendException( "childException" )
      got <- class( default )
      want <- c("childException", "Exception", "condition")
      expect_equal( got, want )
   })
   it( "Extending an exception overrides the message, if specified", {
      e <- extendException( "NewException", base=condition() )
      got <- conditionMessage( e )
      want <- "condition"
      expect_equal( got, want )

      e <- extendException( "NewException", base=condition("Inherited message") )
      got <- conditionMessage( e )
      want <- "Inherited message"
      expect_equal( got, want )

      e <- extendException( "NewException", base=condition(),
                            message= "New Exception message" )
      got <- conditionMessage( e )
      want <- "New Exception message"
      expect_equal( got, want )

      e <- extendException( "NewException", base=condition("Inherited message"),
                            message= "New Exception message" )
      got <- conditionMessage( e )
      want <- "New Exception message"
      expect_equal( got, want )

   })
   it( "Extending an exception overrides the call, if specified", {
      aCall <- sys.call()
      bCall <- sys.calls()
      expect_false( isTRUE(all.equal(aCall, bCall)))

      e <- extendException( "NewException", base=condition() )
      got <- conditionCall( e )
      expect_null( got )

      e <- extendException( "NewException", base=condition( call= aCall ))
      got <- conditionCall( e )
      want <- aCall
      expect_equal( got, want )

      e <- extendException( "NewException", base=condition(), call= bCall)
      got <- conditionCall( e )
      want <- bCall
      expect_equal( got, want )

      e <- extendException( "NewException", base=condition( call= aCall ), call= bCall)
      got <- conditionCall( e )
      want <- bCall
      expect_equal( got, want )
   })
   it( "Extending an exception sets or overrides data arguments, if specified", {

      e <- extendException( "NewException", base=condition(),
                            arg1= "arg1 value", arg2= "arg2 value" )
      got <- c(e$arg1, e$arg2, e$arg3)
      want <- c("arg1 value", "arg2 value", NULL)
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e )
      got <- c(e$arg1, e$arg2, e$arg3)
      want <- c("arg1 value", "arg2 value", NULL)
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e, arg3= "arg3 value" )
      got <- c(ee$arg1, ee$arg2, ee$arg3)
      want <- c("arg1 value", "arg2 value", "arg3 value")
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e, arg1= "new arg1 value" )
      got <- c(ee$arg1, ee$arg2, ee$arg3)
      want <- c("new arg1 value", "arg2 value", NULL)
      expect_equal( got, want )

      ee <- extendException( "NewNewException", base=e,
                             arg1= "new arg1 value", arg3= "arg3 value" )
      got <- c(ee$arg1, ee$arg2, ee$arg3)
      want <- c("new arg1 value", "arg2 value", "arg3 value")
      expect_equal( got, want )

   })
})

describe( "Exception() object constructor", {

   defaultException <- Exception()
   messageException <- Exception( message= "a message" )

   aCallStack <- sys.calls()
   callException <- Exception( message="Now with calls.", call= aCallStack)

   it( "generates  'condition' class object", {
      expect_equal( class( defaultException ), c('Exception', 'condition') )
      expect_equal( class( messageException ), c('Exception', 'condition') )
      expect_equal( class(    callException ), c('Exception', 'condition') )
   })
   it( "generates objects with the expected message content", {
      expect_equal( conditionMessage( defaultException ),       'Exception' )
      expect_equal( conditionMessage( messageException ),       'a message' )
      expect_equal( conditionMessage(    callException ), 'Now with calls.' )
   })
   it( "generates objects with the expected calls content", {
      expect_null(  conditionCall( defaultException ))
      expect_null(  conditionCall( messageException ))
      expect_equal( conditionCall( callException ), aCallStack )
   })
})

describe( "FileException() object constructor", {

   defaultException <- FileException( path="default" )

   messageException <- FileException( path="message", message= "a message" )

   aCallStack <- sys.calls()
   callException <- FileException( path= "call", message="Now with calls.", call= aCallStack)

   it( "generates  'condition' class object", {
      expect_equal( class( defaultException ), c('FileException', 'Exception', 'condition') )
      expect_equal( class( messageException ), c('FileException', 'Exception', 'condition') )
      expect_equal( class(    callException ), c('FileException', 'Exception', 'condition') )
   })
   it( "generates objects with the expected message content", {
      want <- paste0( 'File Exception: "default" (looking in: "', getwd(), '").')

      expect_equal( conditionMessage( defaultException ),              want )
      expect_equal( conditionMessage( messageException ),       'a message' )
      expect_equal( conditionMessage(    callException ), 'Now with calls.' )
   })
   it( "generates objects with the expected calls content", {
      expect_null(  conditionCall( defaultException ))
      expect_null(  conditionCall( messageException ))
      expect_equal( conditionCall( callException ), aCallStack )
   })
   it( "generates objects with the expected path content", {
      expect_equal( path( defaultException ), "default" )
      expect_equal( path( messageException ), "message" )
      expect_equal( path(    callException ),    "call" )
   })
})

describe( "FileNotFoundException() object constructor", {

   defaultException <- FileNotFoundException( path="default" )
   messageException <- FileNotFoundException( path="message", message= "a message" )

   aCallStack <- sys.calls()
   callException <- FileNotFoundException( path= "call", call= aCallStack)

   it( "generates  'condition' class object", {
      expect_equal( class( defaultException ),
                    c('FileNotFoundException', 'FileException', 'Exception', 'condition') )
      expect_equal( class( messageException ),
                    c('FileNotFoundException', 'FileException', 'Exception', 'condition') )
      expect_equal( class(    callException ),
                    c('FileNotFoundException', 'FileException', 'Exception', 'condition') )
   })
   it( "generates objects with the expected message content", {
      want <- paste0( 'File not found: "default" (looking in: "', getwd(), '").')
      expect_equal( conditionMessage( defaultException ),        want )
      expect_equal( conditionMessage( messageException ), 'a message' )
      want <- paste0( 'File not found: "call" (looking in: "', getwd(), '").')
      expect_equal( conditionMessage(    callException ), want )
   })
   it( "generates objects with the expected calls content", {
      expect_null(  conditionCall( defaultException ))
      expect_null(  conditionCall( messageException ))
      expect_equal( conditionCall( callException ), aCallStack )
   })
   it( "generates objects with the expected path content", {
      expect_equal( path( defaultException ), "default" )
      expect_equal( path( messageException ), "message" )
      expect_equal( path(    callException ),    "call" )
   })
})
