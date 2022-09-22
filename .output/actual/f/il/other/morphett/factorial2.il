[ Comment " Factorial version 2 "
, Lambda
    [ Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Inst
        ( IAL ( Binary Mul ) )
    , Inst
        ( IAL
            ( SStatic 1 Move )
        )
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
        [ Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL
                ( SStatic 0 Copy )
            )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL
                ( SStatic 2 Move )
            )
        , Inst
            ( IAL
                ( SStatic 2 Move )
            )
        , Inst
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