[ Comment " factorial program -- from original False distribution "
, Lambda
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
, Comment " factorial() function in False "
, Str "calculate the factorial of [1..8]: "
, Ref 1
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 1
, Inst
    ( IAL
        ( Cons 48 )
    )
, Inst
    ( IAL ( Binary Sub ) )
, Inst ( IAL Dup )
, Inst ( IAL Dup )
, Inst
    ( IAL
        ( Cons 0 )
    )
]