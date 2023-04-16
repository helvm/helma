[ Comment " this program tests the argument facility of the portable interpreter "
, Ref 0
, Fetch
, Inst
    ( ISM
        ( SPure
            ( Cons 2 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary LEQ ) )
    )
, Inst
    ( ISM
        ( SPure ( Unary BNot ) )
    )
, Inst
    ( ISM
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
    ( ISM
        ( SPure ( Unary BNot ) )
    )
, Lambda
    [ Ref 1
    , Fetch
    , Inst
        ( ISM ( SIO OutputDec ) )
    , Str " + "
    , Ref 2
    , Fetch
    , Inst
        ( ISM ( SIO OutputDec ) )
    , Str " = "
    , Ref 1
    , Fetch
    , Ref 2
    , Fetch
    , Inst
        ( ISM
            ( SPure ( Binary Add ) )
        )
    , Inst
        ( ISM ( SIO OutputDec ) )
    , Str "
      "
    ]
, Cond
]