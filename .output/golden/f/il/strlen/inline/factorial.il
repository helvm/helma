[ Comment " factorial program in false! "
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
            ( SAL ( Binary LEQ ) )
        )
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
, Comment " fac() in false "
, Str "calculate the factorial of [1..8]: "
, Flush
, Inst
    ( IAL ( SIO InputChar ) )
, Flush
, Inst
    ( IAL
        ( SAL
            ( Cons 48 )
        )
    )
, Inst
    ( IAL
        ( SAL ( Binary Sub ) )
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
            ( SStatic 0 Copy )
        )
    )
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
]