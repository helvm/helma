[ Comment "countwl.f"
, Comment "count lines and words "
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
, Ref 1
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 0 )
        )
    )
, Ref 2
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
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Lambda
        [ Ref 0
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
        ]
    , Cond
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
    , Ref 3
    , Store
    , Inst
        ( IAL
            ( SAL
                ( Cons 32 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Ref 3
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 32 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary BOr ) )
        )
    , Ref 3
    , Fetch
    , Inst
        ( IAL
            ( SAL
                ( Cons 10 )
            )
        )
    , Inst
        ( IAL
            ( SAL ( Binary LEQ ) )
        )
    , Inst
        ( IAL
            ( SAL ( Binary BOr ) )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Lambda
        [ Ref 2
        , Fetch
        , Inst
            ( IAL
                ( SAL ( Unary BNot ) )
            )
        , Lambda
            [ Ref 1
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
            , Ref 2
            , Store
            ]
        , Cond
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
                    ( Cons 0 )
                )
            )
        , Ref 2
        , Store
        ]
    , Cond
    , Inst
        ( IAL
            ( SAL ( Binary BOr ) )
        )
    ]
, Ref 0
, Fetch
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
, Ref 0
, Store
, Ref 1
, Fetch
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
, Ref 1
, Store
, While
, Str "Number of words: "
, Ref 1
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  Number of lines: "
, Ref 0
, Fetch
, Inst
    ( IAL ( SIO OutputDec ) )
, Str "
  "
]