[ Lambda
    [ Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        , Inst
            ( IAL ( SAL Discard ) )
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL ( Unary BNot ) )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Sub ) )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
                ( SAL ( Binary Mul ) )
            )
        ]
    , Cond
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 4 )
        )
    )
, Ref 5
, Fetch
, Exec
, Inst
    ( IAL ( SIO OutputDec ) )
]