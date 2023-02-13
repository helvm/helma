[ Comment " tic tac toe "
, Comment " written in false "
, Comment " by Marcel van Kervinck "
, Comment " e-mail <marcelk@stack.urc.tue.nl> "
, Comment " June 1994 "
, Comment " for all "
, Lambda
    [ Lambda
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
                    ( Indexed Move
                        ( ImmediateO 2 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary BAnd ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Cons 2 )
                )
            )
        , Ref 14
        , Exec
        ]
    , While
    , Inst
        ( IAL ( SPure Discard ) )
    , Inst
        ( IAL ( SPure Discard ) )
    ]
, Ref 5
, Store
, Comment " print board "
, Lambda
    [ Lambda
        [ Str "   "
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Copy
                        ( ImmediateO 0 )
                    )
                )
            )
        , Ref 23
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Div ) )
            )
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        , Ref 14
        , Fetch
        , Inst
            ( IAL
                ( SPure
                    ( Indexed Move
                        ( ImmediateO 1 )
                    )
                )
            )
        , Inst
            ( IAL
                ( SPure ( Binary Div ) )
            )
        , Lambda
            [ Inst
                ( IAL
                    ( SPure
                        ( Cons 32 )
                    )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
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
                        ( Cons 4 )
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
            , Inst
                ( IAL
                    ( SPure
                        ( Indexed Copy
                            ( ImmediateO 0 )
                        )
                    )
                )
            , Lambda
                [ Str "O"
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
            , Cond
            , Inst
                ( IAL
                    ( SPure ( Unary BNot ) )
                )
            , Lambda
                [ Inst
                    ( IAL
                        ( SPure
                            ( Cons 4 )
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
                , Inst
                    ( IAL
                        ( SPure
                            ( Indexed Copy
                                ( ImmediateO 0 )
                            )
                        )
                    )
                , Lambda
                    [ Str "X" ]
                , Cond
                , Inst
                    ( IAL
                        ( SPure ( Unary BNot ) )
                    )
                , Lambda
                    [ Str "." ]
                , Cond
                ]
            , Cond
            ]
        , Inst
            ( IAL
                ( SPure
                    ( Cons 7 )
                )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
                ( SPure
                    ( Cons 10 )
                )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        , Inst
            ( IAL ( SPure Discard ) )
        , Inst
            ( IAL ( SPure Discard ) )
        ]
    , Str "7 8 9"
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
                ( Cons 64 )
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
    , Exec
    , Str "4 5 6"
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
                ( Cons 8 )
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
    , Exec
    , Str "1 2 3"
    , Inst
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
    , Exec
    ]
, Ref 1
, Store
, Comment " test three-in-a-row "
]