[ Lambda
    [ Inst ( IAL Dup )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Inst ( IAL Dup )
    , Lambda
        [ Inst ( IAL Swap )
        , Inst ( IAL Discard )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst ( IAL Swap )
        ]
    , Cond
    , Inst
        ( IAL ( Unary BNot ) )
    , Lambda
        [ Inst ( IAL Dup )
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