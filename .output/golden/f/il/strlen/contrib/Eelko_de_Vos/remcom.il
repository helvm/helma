[ Comment "remcom.f"
, Comment "removes comments like this..."
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 2
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 3
, Store
, Inst
    ( IAL
        ( Cons 0 )
    )
, Ref 4
, Store
, Lambda
    [ Inst
        ( IAL ( SIO InputChar ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 34 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Ref 4
        , Fetch
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Add ) )
        , Ref 4
        , Store
        ]
    , Cond
    , Ref 1
    , Store
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Unary Neg ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL ( Unary BNot ) )
    ]
]