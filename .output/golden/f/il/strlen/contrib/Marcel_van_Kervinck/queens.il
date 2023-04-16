[ Comment " queens "
, Comment " solves the problem of placing n queens on a "
, Comment " n*n chessboard without attacking eachother "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
, Lambda
    [ Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Inst
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
            ( SPure
                ( Indexed
                    ( IImmediate 0 ) Copy
                )
            )
        )
    , Lambda
        [ Inst
            ( ISM
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        ]
    , Cond
    , Inst
        ( ISM
            ( SPure ( Unary BNot ) )
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
                ( SPure
                    ( Cons 2 )
                )
            )
        , Ref 14
        , Lambda
            [ Inst
                ( ISM
                    ( SPure
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Inst
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
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Inst
                ( ISM
                    ( SPure
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Inst
                ( ISM
                    ( SPure ( Unary Neg ) )
                )
            , Inst
                ( ISM
                    ( SPure ( Binary BAnd ) )
                )
            , Inst
                ( ISM
                    ( SPure
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Inst
                ( ISM
                    ( SPure
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( ISM
                    ( SPure
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( ISM
                    ( SPure ( Binary BOr ) )
                )
            , Inst
                ( ISM
                    ( SPure ( Unary Neg ) )
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
            , Inst
                ( ISM
                    ( SPure ( Binary BAnd ) )
                )
            , Lambda
                [ Inst
                    ( ISM
                        ( SPure
                            ( Indexed
                                ( IImmediate 2 ) Move
                            )
                        )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 1 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure ( Binary BOr ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Indexed
                                ( IImmediate 0 ) Copy
                            )
                        )
                    )
                , Inst
                    ( ISM
                        ( SPure ( Binary Add ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 2 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure ( Binary BOr ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 2 )
                        )
                    )
                , Inst
                    ( ISM
                        ( SPure ( Binary Div ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 3 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure ( Unary Neg ) )
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
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( ISM
                        ( SPure ( Binary BAnd ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Cons 6 )
                        )
                    )
                , Ref 14
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
                , Ref 16
                , Fetch
                , Exec
                , Inst
                    ( ISM
                        ( SPure ( Binary Add ) )
                    )
                , Inst
                    ( ISM
                        ( SPure
                            ( Indexed
                                ( IImmediate 2 ) Move
                            )
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
                ]
            , Cond
            , Inst
                ( ISM
                    ( SPure ( Unary Neg ) )
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
            , Inst
                ( ISM
                    ( SPure ( Binary BAnd ) )
                )
            ]
        , While
        , Inst
            ( ISM ( SPure Discard ) )
        ]
    , Cond
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM
            ( SPure
                ( Indexed
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Inst
        ( ISM ( SPure Discard ) )
    ]
, Ref 16
, Store
, Str "enter number of queens [0..9] "
, Ref 1
, Inst
    ( ISM ( SIO InputChar ) )
, Ref 1
, Inst
    ( ISM
        ( SPure
            ( Cons 48 )
        )
    )
, Inst
    ( ISM
        ( SPure ( Binary Sub ) )
    )
, Inst
    ( ISM
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
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
, Inst
    ( ISM
        ( SPure
            ( Indexed
                ( IImmediate 0 ) Copy
            )
        )
    )
, Inst
    ( ISM
        ( SPure
            ( Cons 1 )
        )
    )
]