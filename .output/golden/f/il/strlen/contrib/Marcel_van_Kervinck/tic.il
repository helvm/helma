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
                    ( SStatic 0 Copy )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Unary Neg ) )
            )
        , Inst
            ( IAL
                ( SAL ( Binary BAnd ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Unary Neg ) )
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
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 2 Move )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary BAnd ) )
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
                    ( Cons 2 )
                )
            )
        , Ref 14
        , Exec
        ]
    , While
    , Inst
        ( IAL ( SAL Discard ) )
    , Inst
        ( IAL ( SAL Discard ) )
    ]
, Ref 5
, Store
, Comment " print board "
, Lambda
    [ Lambda
        [ Str "   "
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 0 Copy )
                )
            )
        , Ref 23
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Div ) )
            )
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        , Ref 14
        , Fetch
        , Inst
            ( IAL
                ( SAL
                    ( SStatic 1 Move )
                )
            )
        , Inst
            ( IAL
                ( SAL ( Binary Div ) )
            )
        , Lambda
            [ Inst
                ( IAL
                    ( SAL
                        ( Cons 32 )
                    )
                )
            , Inst
                ( IAL ( SIO OutputChar ) )
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Inst
                ( IAL
                    ( SAL
                        ( Cons 4 )
                    )
                )
            , Ref 14
            , Inst
                ( IAL
                    ( SAL ( Binary BAnd ) )
                )
            , Inst
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
            , Inst
                ( IAL
                    ( SAL
                        ( SStatic 0 Copy )
                    )
                )
            , Lambda
                [ Str "O"
                , Inst
                    ( IAL
                        ( SAL
                            ( SStatic 1 Move )
                        )
                    )
                , Inst
                    ( IAL ( SAL Discard ) )
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
                            ( Cons 4 )
                        )
                    )
                , Ref 14
                , Inst
                    ( IAL
                        ( SAL ( Binary BAnd ) )
                    )
                , Inst
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
                , Inst
                    ( IAL
                        ( SAL
                            ( SStatic 0 Copy )
                        )
                    )
                , Lambda
                    [ Str "X" ]
                , Cond
                , Inst
                    ( IAL
                        ( SAL ( Unary BNot ) )
                    )
                , Lambda
                    [ Str "." ]
                , Cond
                ]
            , Cond
            ]
        , Inst
            ( IAL
                ( SAL
                    ( Cons 7 )
                )
            )
        , Ref 5
        , Fetch
        , Exec
        , Inst
            ( IAL
                ( SAL
                    ( Cons 10 )
                )
            )
        , Inst
            ( IAL ( SIO OutputChar ) )
        , Inst
            ( IAL ( SAL Discard ) )
        , Inst
            ( IAL ( SAL Discard ) )
        ]
    , Str "7 8 9"
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 64 )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Exec
    , Str "4 5 6"
    , Inst
        ( IAL
            ( SAL
                ( SStatic 0 Copy )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( Cons 8 )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Exec
    , Str "1 2 3"
    , Inst
        ( IAL
            ( SAL
                ( Cons 1 )
            )
        )
    , Inst
        ( IAL
            ( SAL
                ( SStatic 1 Move )
            )
        )
    , Exec
    ]
, Ref 1
, Store
, Comment " test three-in-a-row "
]