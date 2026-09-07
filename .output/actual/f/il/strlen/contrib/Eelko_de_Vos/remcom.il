[ Comment "remcom.f"
, Comment "removes comments like this..."
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 3
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 4
, Store
, Lambda
    [ Inst
        ( ISM ( SIO InputChar ) )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 34 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 4
        , Fetch
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Ref 4
        , Store
        ]
    , Cond
    , Ref 1
    , Store
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure ( Unary Neg ) )
        )
    , Inst
        ( ISM
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
        )
    ]
]