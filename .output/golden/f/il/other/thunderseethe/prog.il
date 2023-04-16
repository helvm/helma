[ Comment "[n: 0 1 [n;0=~][$2o+n;1-n:]# %%]f: 300f;!"
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Cons 0 )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Cons 1 )
            )
        )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 2 ) Move
                )
            )
        )
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 0 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary LEQ ) )
            )
        , Inst
            ( ISM
                ( SPure ( Unary BNot ) )
            )
        ]
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Ref 14
        , Inst
            ( ISM
                ( SPure
                    ( Cons 3 )
                )
            )
        , Ref 14
        , Inst
            ( ISM
                ( SPure ( Binary Add ) )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary Sub ) )
            )
        ]
    , While
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM ( SPure Discard ) )
    ]
, Ref 5
, Store
, Inst
    ( ISM
        ( SPure
            ( Cons 300 )
        )
    )
, Ref 5
, Fetch
, Exec
, Comment "4 0 1 @ 1o 3o + \ 1 -"
]