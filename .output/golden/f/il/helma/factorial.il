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
, Comment " fac() in false "
, Str "calculate the factorial of [1..8]: "
, Flush
, Inst
    ( IAL ( SIO InputChar ) )
, Flush
, Inst
    ( IAL
        ( Cons 48 )
    )
, Inst
    ( IAL ( Binary Sub ) )
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
        ( Cons 0 )
    )
]