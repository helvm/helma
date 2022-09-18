[ Comment " Factorial version 2 "
, Lambda
    [ Inst ( IAL Rot )
    , Inst
        ( IAL ( Binary Mul ) )
    , Inst ( IAL Swap )
    , Exec
    ]
, Ref 0
, Store
, Lambda
    [ Lambda
        [ Inst
            ( IAL ( SIO OutputDec ) )
        ]
    , Lambda
        [ Inst ( IAL Swap )
        , Inst ( IAL Dup )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst ( IAL Swap )
        , Inst ( IAL Rot )
        , Inst ( IAL Rot )
        , Inst ( IAL Dup )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Unary BNot ) )
        ]
    , Lambda
        [ Ref 0
        , Fetch
        ]
    , While
    , Ref 0
    , Fetch
    , Exec
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
]