[ Comment " queens "
, Comment " solves the problem of placing n queens on a "
, Comment " n*n chessboard without attacking eachother "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
, Lambda
    [ Inst
        ( IAL
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Inst
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
            ( SPure
                ( Indexed Copy
                    ( ImmediateO 0 )
                )
            )
        )
    , Lambda
        [ Inst
            ( IAL
                ( SPure
                    ( Cons 1 )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure ( Unary BNot ) )
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
                ( SPure
                    ( Cons 2 )
                )
            )
        , Ref 14
        , Lambda
            [ Inst
                ( IAL
                    ( SPure
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Inst
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
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Inst
                ( IAL
                    ( SPure
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Inst
                ( IAL
                    ( SPure ( Unary Neg ) )
                )
            , Inst
                ( IAL
                    ( SPure ( Binary BAnd ) )
                )
            , Inst
                ( IAL
                    ( SPure
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Inst
                ( IAL
                    ( SPure
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( SPure
                        ( Cons 7 )
                    )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( SPure ( Binary BOr ) )
                )
            , Inst
                ( IAL
                    ( SPure ( Unary Neg ) )
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
            , Inst
                ( IAL
                    ( SPure ( Binary BAnd ) )
                )
            , Lambda
                [ Inst
                    ( IAL
                        ( SPure
                            ( Indexed Move
                                ( ImmediateO 2 )
                            )
                        )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 1 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure ( Binary BOr ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Indexed Copy
                                ( ImmediateO 0 )
                            )
                        )
                    )
                , Inst
                    ( IAL
                        ( SPure ( Binary Add ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 2 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure ( Binary BOr ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 2 )
                        )
                    )
                , Inst
                    ( IAL
                        ( SPure ( Binary Div ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 3 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure ( Unary Neg ) )
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
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 7 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SPure ( Binary BAnd ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Cons 6 )
                        )
                    )
                , Ref 14
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
                , Ref 16
                , Fetch
                , Exec
                , Inst
                    ( IAL
                        ( SPure ( Binary Add ) )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Indexed Move
                                ( ImmediateO 2 )
                            )
                        )
                    )
                , Inst
                    ( IAL
                        ( SPure
                            ( Indexed Move
                                ( ImmediateO 2 )
                            )
                        )
                    )
                ]
            , Cond
            , Inst
                ( IAL
                    ( SPure ( Unary Neg ) )
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
            , Inst
                ( IAL
                    ( SPure ( Binary BAnd ) )
                )
            ]
        , While
        , Inst
            ( IAL ( SPure Discard ) )
        ]
    , Cond
    , Inst
        ( IAL
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL
            ( SPure
                ( Indexed Move
                    ( ImmediateO 1 )
                )
            )
        )
    , Inst
        ( IAL ( SPure Discard ) )
    ]
, Ref 16
, Store
, Str "enter number of queens [0..9] "
, Ref 1
, Inst
    ( IAL ( SIO InputChar ) )
, Ref 1
, Inst
    ( IAL
        ( SPure
            ( Cons 48 )
        )
    )
, Inst
    ( IAL
        ( SPure ( Binary Sub ) )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 0 )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Move
                ( ImmediateO 2 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Indexed Copy
                ( ImmediateO 0 )
            )
        )
    )
, Inst
    ( IAL
        ( SPure
            ( Cons 1 )
        )
    )
]