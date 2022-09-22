[ Comment "[n: 0 1 [n;0=~][$2o+n;1-n:]# %%]f: 300f;!"
, Lambda
    [ Inst
        ( IAL
            ( Cons 0 )
        )
    , Inst
        ( IAL
            ( Cons 1 )
        )
    , Inst
        ( IAL
            ( SStatic 2 Move )
        )
    , Lambda
        [ Inst
            ( IAL
                ( Cons 0 )
            )
        , Inst
            ( IAL ( Binary LEQ ) )
        , Inst
            ( IAL ( Unary BNot ) )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( Cons 1 )
            )
        , Ref 14
        , Inst
            ( IAL
                ( Cons 3 )
            )
        , Ref 14
        , Inst
            ( IAL ( Binary Add ) )
        , Inst
            ( IAL
                ( SStatic 1 Move )
            )
        , Inst
            ( IAL
                ( Cons 1 )
            )
        , Inst
            ( IAL ( Binary Sub ) )
        ]
    , While
    , Inst ( IAL Discard )
    , Inst ( IAL Discard )
    , Inst ( IAL Discard )
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( Cons 300 )
    )
, Ref 5
, Fetch
, Exec
, Comment "4 0 1 @ 1o 3o + \ 1 -"
]