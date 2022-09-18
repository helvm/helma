[ Comment " writes all prime numbers between 0 and 100 "
, Inst
    ( IAL
        ( Cons 99 )
    )
, Inst
    ( IAL
        ( Cons 9 )
    )
, Lambda
    [ Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL ( Binary Sub ) )
    , Inst ( IAL Dup )
    ]
, Lambda
    [ Inst ( IAL Swap )
    , Inst ( IAL Dup )
    , Inst ( IAL Rot )
    , Inst ( IAL Dup )
    , Inst ( IAL Rot )
    , Inst ( IAL Dup )
    , Inst ( IAL Rot )
    , Inst ( IAL Dup )
    , Inst ( IAL Rot )
    , Inst ( IAL Swap )
    , Inst
        ( IAL ( Binary Div ) )
    , Inst
        ( IAL ( Binary Mul ) )
    , Inst
        ( IAL ( Binary LEQ ) )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        , Inst ( IAL Dup )
        , Inst ( IAL Dup )
        , Lambda
            [ Inst ( IAL Discard )
            , Inst ( IAL Swap )
            , Inst
                ( IAL
                    ( Cons 1 )
                )
            , Inst
                ( IAL ( Binary Sub ) )
            , Inst ( IAL Dup )
            , Inst ( IAL Rot )
            ]
        , Cond
        , Inst
            ( IAL
                ( Cons 0 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Lambda
            [ Inst ( IAL Swap )
            , Inst ( IAL Dup )
            , Inst
                ( IAL ( SIO OutputDec ) )
            , Inst
                ( IAL
                    ( Cons 32 )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Inst ( IAL Swap )
            ]
        , Cond
        ]
    , Cond
    ]
, While
]