[ Comment "remcom.f"
, Comment "removes comments like this..."
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 0
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 2
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 3
, Store
, Inst
    ( IAL
        ( SAL
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
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 34 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 4
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Ref 4
        , Store
        ]
    , Cond
    , Ref 1
    , Store
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Unary Neg ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    ]
]