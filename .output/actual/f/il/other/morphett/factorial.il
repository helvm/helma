[ Lambda
    [ Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst
        ( IAL
            ( SStatic 0 Copy )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst ( IAL Discard )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        ]
    , Cond
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL ( Binary Mul ) )
        ]
    , Cond
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( Cons 4 )
    )
, Ref 5
, Fetch
, Exec
, Inst
    ( IAL ( SIO OutputDec ) )
]