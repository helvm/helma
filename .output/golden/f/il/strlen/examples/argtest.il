[ Comment " this program tests the argument facility of the portable interpreter "
, Ref 0
, Fetch
, Inst
    ( IAL
        ( SPure
            ( Cons 2 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Lambda
    [ Str "2 integer arguments expected
    " ]
, Cond
, Inst
    ( IAL
        ( SPure ( Unary BNot ) )
    )
, Lambda
    [ Ref 1
    , Fetch
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str " + "
    , Ref 2
    , Fetch
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str " = "
    , Ref 1
    , Fetch
    , Ref 2
    , Fetch
    , Inst
        ( IAL
            ( SPure ( Binary Add ) )
        )
    , Inst
        ( IAL ( SIO OutputDec ) )
    , Str "
      "
    ]
, Cond
]