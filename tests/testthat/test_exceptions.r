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
      expect_equal( conditionMessage( defaultException ),   'FileException' )
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
