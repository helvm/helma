[ Comment "[n: 0 1 [n;0=~][$2o+n;1-n:]# %%]f: 300f;!"
, Lambda
    [ Inst
        ( IAL
            ( SAL
                ( Cons 0 )
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
            ( SAL
                ( SStatic 2 Move )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 0 )
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
        ]
    , Lambda
        [ Inst
            ( IAL
                ( SAL
                    ( Cons 1 )
                )
            )
        , Ref 14
        , Inst
            ( IAL
                ( SAL
                    ( Cons 3 )
                )
            )
        , Ref 14
        , Inst
            ( IAL
                ( SAL ( Binary Add ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
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
        ]
    , While
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL ( SAL Discard ) )
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( SAL
            ( Cons 300 )
        )
    )
, Ref 5
, Fetch
, Exec
, Comment "4 0 1 @ 1o 3o + \ 1 -"
]