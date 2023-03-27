[ Comment "[n: 0 1 [n;0=~][$2o+n;1-n:]# %%]f: 300f;!"
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Cons 0 )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SPure
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 0 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( IAL
                ( SPure ( Unary BNot ) )
            )
        ]
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Ref 14
        , Inst
            ( IAL
                ( SPure
                    ( Cons 3 )
                )
            )
        , Ref 14
        , Inst
            ( IAL
                ( SPure ( Binary Add ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Sub ) )
            )
        ]
    , While
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL ( SPure Discard ) )
    ]
, Ref 5
, Store
, Inst
    ( IAL
        ( SPure
            ( Cons 300 )
        )
    )
, Ref 5
, Fetch
, Exec
, Comment "4 0 1 @ 1o 3o + \ 1 -"
]