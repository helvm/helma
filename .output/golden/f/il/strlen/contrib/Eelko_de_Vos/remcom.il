[ Comment "remcom.f"
, Comment "removes comments like this..."
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 3
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Ref 4
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 34 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 4
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Ref 4
        , Store
        ]
    , Cond
    , Ref 1
    , Store
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SPure ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
        )
    ]
]