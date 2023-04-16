[ Comment " tic tac toe "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
, Comment " for all "
, Lambda
    [ Lambda
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
                    ( Indexed
                        ( IImmediate 2 ) Move
                    )
                )
            )
        , Inst
            ( ISM
                ( SPure ( Binary BAnd ) )
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
                    ( Cons 2 )
                )
            )
        , Ref 14
        , Exec
        ]
    , While
    , Inst
        ( ISM ( SPure Discard ) )
    , Inst
        ( ISM ( SPure Discard ) )
    ]
, Ref 5
, Store
, Comment " print board "
, Lambda
    [ Lambda
        [ Str "   "
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 0 ) Copy
                    )
                )
            )
        , Ref 23
        , Fetch
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
                ( SPure ( Binary Div ) )
            )
        , Inst
            ( ISM
                ( SPure
                    ( Indexed
                        ( IImmediate 1 ) Move
                    )
                )
            )
        , Ref 14
        , Fetch
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
                ( SPure ( Binary Div ) )
            )
        , Lambda
            [ Inst
                ( ISM
                    ( SPure
                        ( Cons 32 )
                    )
                )
            , Inst
                ( ISM ( SIO OutputChar ) )
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
                        ( Cons 4 )
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
            , Inst
                ( ISM
                    ( SPure
                        ( Indexed
                            ( IImmediate 0 ) Copy
                        )
                    )
                )
            , Lambda
                [ Str "O"
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
            , Cond
            , Inst
                ( ISM
                    ( SPure ( Unary BNot ) )
                )
            , Lambda
                [ Inst
                    ( ISM
                        ( SPure
                            ( Cons 4 )
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
                , Inst
                    ( ISM
                        ( SPure
                            ( Indexed
                                ( IImmediate 0 ) Copy
                            )
                        )
                    )
                , Lambda
                    [ Str "X" ]
                , Cond
                , Inst
                    ( ISM
                        ( SPure ( Unary BNot ) )
                    )
                , Lambda
                    [ Str "." ]
                , Cond
                ]
            , Cond
            ]
        , Inst
            ( ISM
                ( SPure
                    ( Cons 7 )
                )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( ISM
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( ISM ( SIO OutputChar ) )
        , Inst
            ( ISM ( SPure Discard ) )
        , Inst
            ( ISM ( SPure Discard ) )
        ]
    , Str "7 8 9"
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
                ( Cons 64 )
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
    , Exec
    , Str "4 5 6"
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
                ( Cons 8 )
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
    , Exec
    , Str "1 2 3"
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
                    ( IImmediate 1 ) Move
                )
            )
        )
    , Exec
    ]
, Ref 1
, Store
, Comment " test three-in-a-row "
]